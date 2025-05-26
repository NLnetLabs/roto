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

pub mod context;
pub mod func;
pub mod layout;
pub mod optional;
pub mod ty;
pub mod val;
pub mod verdict;

use core::{slice, str};
use std::{
    any::{type_name, TypeId},
    collections::HashMap,
    net::{IpAddr, Ipv4Addr, Ipv6Addr},
    ptr,
    sync::Arc,
};

use context::ContextDescription;
use func::{Func, FunctionDescription};
use inetnum::{addr::Prefix, asn::Asn};
use layout::Layout;
use roto_macros::{roto_method, roto_static_method};
use ty::{Ty, TypeDescription, TypeRegistry};

use crate::{ast::Identifier, Context};

/// Provides the types and functions that Roto can access via FFI
///
/// Even some types that can be written as literals should be provided here.
/// The idea here is that Roto can be used with different representations
/// of these types in different applications. The type checker will yield an
/// error if a literal is provided for an undeclared type.
pub struct Runtime {
    pub context: Option<ContextDescription>,
    pub runtime_types: Vec<RuntimeType>,
    pub functions: Vec<RuntimeFunction>,
    pub constants: HashMap<Identifier, RuntimeConstant>,
    pub type_registry: TypeRegistry,
    pub string_init_function:
        unsafe extern "C" fn(*mut Arc<str>, *mut u8, u32),
}

#[derive(Debug)]
pub enum Movability {
    // This type is passed by value, only available for built-in types.
    Value,

    // This type can be copied without calling clone and drop.
    Copy,

    // This type needs a clone and drop function.
    CloneDrop(CloneDrop),
}

#[derive(Debug)]
pub struct CloneDrop {
    pub clone: unsafe extern "C" fn(*const (), *mut ()),
    pub drop: unsafe extern "C" fn(*mut ()),
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
    unsafe { std::ptr::drop_in_place(x) };
}

unsafe extern "C" fn init_string(s: *mut Arc<str>, data: *mut u8, len: u32) {
    let slice = unsafe { slice::from_raw_parts(data, len as usize) };
    let str = unsafe { str::from_utf8_unchecked(slice) };
    unsafe { ptr::write(s, str.into()) };
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

    /// Layout of the type
    layout: Layout,

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

    pub fn layout(&self) -> Layout {
        self.layout.clone()
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RuntimeFunctionRef(usize);

impl RuntimeFunction {
    pub fn get_ref(&self) -> RuntimeFunctionRef {
        RuntimeFunctionRef(self.id)
    }
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
    pub docstring: String,
    pub bytes: Box<[u8]>,
}

impl Runtime {
    pub fn get_function(&self, f: RuntimeFunctionRef) -> &RuntimeFunction {
        &self.functions[f.0]
    }

    /// Register a type with a default name
    ///
    /// This type will be cloned and dropped many times, so make sure to have
    /// a cheap [`Clone`] and [`Drop`] implementations, for example an
    /// [`Rc`](std::rc::Rc) or an [`Arc`].
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

    /// Register a type that is passed by value with a default name
    ///
    /// The functions registering types by value cannot be made public, because
    /// the compiler needs special knowledge about them.
    ///
    /// See [`Runtime::register_clone_type`]
    fn register_value_type<T: Copy + 'static>(
        &mut self,
        docstring: &str,
    ) -> Result<(), String> {
        let name = Self::extract_name::<T>();
        self.register_value_type_with_name::<T>(name, docstring)
    }

    fn register_value_type_with_name<T: Copy + 'static>(
        &mut self,
        name: &str,
        docstring: &str,
    ) -> Result<(), String> {
        self.register_type_with_name_internal::<T>(
            name,
            Movability::Value,
            docstring,
        )
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
        let movability = Movability::CloneDrop(CloneDrop {
            clone: extern_clone::<T> as _,
            drop: extern_drop::<T> as _,
        });
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
    /// The `Copy`-ness is not checked. Which is why this is a private function
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
            layout: Layout::of::<T>(),
            docstring: String::from(docstring),
        });
        Ok(())
    }

    pub fn register_context_type<Ctx: Context + 'static>(
        &mut self,
    ) -> Result<(), String> {
        if self.context.is_some() {
            return Err("Only 1 context type can be set".into());
        }

        let description = Ctx::description();

        // The context type likely hasn't been registered yet in the
        // type registry, so we do that, so that we can reason about
        // it more easily and make better error messages.
        self.type_registry.resolve::<*mut Ctx>();

        // All fields in the context must be known because they'll
        // be accessible from Roto.
        for field in &description.fields {
            self.find_type(field.type_id, field.type_name)?;
        }

        self.context = Some(description);

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
        let name = name.into();
        self.check_description(&description)?;

        let id = self.functions.len();
        self.functions.push(RuntimeFunction {
            name,
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
        let name = name.into();
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
                ));
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
            name,
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
        name: impl Into<String>,
        docstring: &str,
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

        let symbol = Identifier::from(name.into());
        self.constants.insert(
            symbol,
            RuntimeConstant {
                name: symbol,
                ty: type_id,
                docstring: docstring.into(),
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

        let mut parameter_types = description.parameter_types().iter();
        let Some(out_pointer_type_id) = parameter_types.next() else {
            return Err("Out parameter missing".to_string());
        };

        let out_pointer_ty =
            self.type_registry.get(*out_pointer_type_id).unwrap();
        match out_pointer_ty.description {
            TypeDescription::MutPtr(id) => {
                let Some(_) =
                    self.runtime_types.iter().find(|ty| ty.type_id == id)
                else {
                    let ty = self.type_registry.get(id).unwrap();
                    return Err(format!(
                        "Registered a function using an unregistered type: `{}`",
                        ty.rust_name
                    ));
                };
            }
            _ => return Err("Out pointer must be a `*mut`".to_string()),
        }

        for type_id in parameter_types {
            let ty = self.type_registry.get(*type_id).unwrap();
            let runtime_ty = check_type(type_id)?;
            if let Movability::Value = runtime_ty.movability {
                if !matches!(ty.description, TypeDescription::Leaf) {
                    let ty = self.type_registry.get(ty.type_id).unwrap();
                    return Err(format!(
                        "Type `{}` should be passed by value. Try removing the `*mut`, `*const`, `&mut` or `&`",
                        ty.rust_name,
                    ));
                }
            } else {
                match ty.description {
                    TypeDescription::MutPtr(_) => {} // correct!
                    TypeDescription::Leaf => {
                        let ty = self.type_registry.get(ty.type_id).unwrap();
                        return Err(format!(
                            "Type `{}` should be passed by pointer. Try adding `*mut`",
                            ty.rust_name,
                        ));
                    }
                    TypeDescription::ConstPtr(_) => {
                        return Err(
                            "Parameters cannot be mutable pointers. Try removing `&` or `*const`".to_string()
                        );
                    }
                    _ => unreachable!("check_type should fail before this"),
                }
                if !matches!(ty.description, TypeDescription::ConstPtr(_)) {}
            }
        }

        if description.return_type() != TypeId::of::<()>() {
            return Err("Runtime function cannot have a return type, use an out pointer instead".into());
        }

        Ok(())
    }

    fn print_ty(&self, ty: TypeId) -> &str {
        let ty = self.get_runtime_type(ty).unwrap();
        ty.name.as_ref()
    }

    fn print_function(&self, f: &RuntimeFunction) {
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
            .map(|ty| self.print_ty(*ty))
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
                format!("{}.", self.print_ty(id))
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

        if let Some(ContextDescription {
            type_id: _,
            type_name: _,
            fields,
        }) = &self.context
        {
            for crate::ContextField {
                name,
                offset: _,
                type_name: _,
                type_id,
                docstring,
            } in fields
            {
                println!(
                    "`````{{roto:context}} {name}: {}",
                    self.print_ty(*type_id)
                );
                for line in docstring.lines() {
                    println!("{line}");
                }
                println!("`````\n");
            }
        }

        for RuntimeConstant {
            name,
            ty,
            docstring,
            ..
        } in self.constants.values()
        {
            println!("`````{{roto:constant}} {name}: {}", self.print_ty(*ty));
            for line in docstring.lines() {
                println!("{line}");
            }
            println!("`````\n");
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
                println!("{line}");
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
    ($t:ty) => {&{
        #[allow(unused_comparisons)]
        let signed = if <$t>::MIN < 0 { "signed" } else { "unsigned" };
        let bits = <$t>::BITS;
        let min = <$t>::MIN;
        let max = <$t>::MAX;
        format!("The {signed} {bits}-bit integer type\n\nThis type can represent integers from {min} up to (and including) {max}.")
    }};
}

macro_rules! float_docs {
    ($t:ty) => {
        &{
            #[allow(unused_comparisons)]
            let bits = std::mem::size_of::<$t>();
            format!("The {bits}-bit floating point type")
        }
    };
}

impl Default for Runtime {
    fn default() -> Self {
        Self::new()
    }
}

macro_rules! float_impl {
    ($rt:ident, $t:ty) => {{
        /// Returns the largest integer less than or equal to self.
        #[roto_method($rt, $t, floor)]
        fn floor(x: $t) -> $t {
            x.floor()
        }

        /// Returns the smallest integer greater than or equal to self.
        #[roto_method($rt, $t, ceil)]
        fn ceil(x: $t) -> $t {
            x.ceil()
        }

        /// Returns the nearest integer to self. If a value is half-way between two integers, round away from 0.0.
        #[roto_method($rt, $t, round)]
        fn round(x: $t) -> $t {
            x.round()
        }

        /// Computes the absolute value of self.
        #[roto_method($rt, $t, abs)]
        fn abs(x: $t) -> $t {
            x.abs()
        }

        /// Returns the square root of a number.
        #[roto_method($rt, $t, sqrt)]
        fn sqrt(x: $t) -> $t {
            x.sqrt()
        }

        /// Raises a number to a floating point power.
        #[roto_method($rt, $t, pow)]
        fn pow(x: $t, y: $t) -> $t {
            x.powf(y)
        }

        /// Returns true if this value is NaN.
        #[roto_method($rt, $t, is_nan)]
        fn is_nan(x: $t) -> bool {
            x.is_nan()
        }

        /// Returns true if this value is positive infinity or negative infinity, and false otherwise.
        #[roto_method($rt, $t, is_infinite)]
        fn is_infinite(x: $t) -> bool {
            x.is_infinite()
        }

        /// Returns true if this number is neither infinite nor NaN.
        #[roto_method($rt, $t, is_finite)]
        fn is_finite(x: $t) -> bool {
            x.is_finite()
        }
    }};
}

impl Runtime {
    /// A Runtime that is as empty as possible.
    ///
    /// This contains only type information for Roto primitives.
    pub fn new() -> Self {
        let mut rt = Runtime {
            context: None,
            runtime_types: Default::default(),
            functions: Default::default(),
            type_registry: Default::default(),
            constants: Default::default(),
            string_init_function: init_string as _,
        };

        rt.register_value_type_with_name::<()>(
            "Unit",
            "The unit type that has just one possible value. It can be used \
            when there is nothing meaningful to be returned.",
        )
        .unwrap();

        rt.register_value_type::<bool>(
            "The boolean type\n\n\
            This type has two possible values: `true` and `false`. Several \
            boolean operations can be used with booleans, such as `&&` (\
            logical and), `||` (logical or) and `not`.",
        )
        .unwrap();

        // All the integer types
        rt.register_value_type::<u8>(int_docs!(u8)).unwrap();
        rt.register_value_type::<u16>(int_docs!(u16)).unwrap();
        rt.register_value_type::<u32>(int_docs!(u32)).unwrap();
        rt.register_value_type::<u64>(int_docs!(u64)).unwrap();
        rt.register_value_type::<i8>(int_docs!(i8)).unwrap();
        rt.register_value_type::<i16>(int_docs!(i16)).unwrap();
        rt.register_value_type::<i32>(int_docs!(i32)).unwrap();
        rt.register_value_type::<i64>(int_docs!(i64)).unwrap();
        rt.register_value_type::<f32>(float_docs!(f32)).unwrap();
        rt.register_value_type::<f64>(float_docs!(f64)).unwrap();

        rt.register_value_type::<Asn>(
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
            ").unwrap();

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
        )
        .unwrap();

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
        ).unwrap();

        float_impl!(rt, f32);
        float_impl!(rt, f64);

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
        fn prefix_new(ip: IpAddr, len: u8) -> Prefix {
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
        fn ipaddr_eq(a: IpAddr, b: IpAddr) -> bool {
            a == b
        }

        /// Returns true if this address is an IPv4 address, and false otherwise.
        ///
        /// ```roto
        /// 1.1.1.1.is_ipv4() # -> true
        /// ::.is_ipv4()      # -> false
        /// ```
        #[roto_method(rt, IpAddr)]
        fn is_ipv4(ip: IpAddr) -> bool {
            ip.is_ipv4()
        }

        /// Returns true if this address is an IPv6 address, and false otherwise.
        ///
        /// ```roto
        /// 1.1.1.1.is_ipv6() # -> false
        /// ::.is_ipv6()      # -> true
        /// ```
        #[roto_method(rt, IpAddr)]
        fn is_ipv6(ip: IpAddr) -> bool {
            ip.is_ipv6()
        }

        /// Converts this address to an IPv4 if it is an IPv4-mapped IPv6 address, otherwise it returns self as-is.
        #[roto_method(rt, IpAddr)]
        fn to_canonical(ip: IpAddr) -> IpAddr {
            ip.to_canonical()
        }

        rt.register_constant(
            "LOCALHOSTV4",
            "The IPv4 address pointing to localhost: `127.0.0.1`",
            IpAddr::from(Ipv4Addr::LOCALHOST),
        )
        .unwrap();

        rt.register_constant(
            "LOCALHOSTV6",
            "The IPv6 address pointing to localhost: `::1`",
            IpAddr::from(Ipv6Addr::LOCALHOST),
        )
        .unwrap();

        rt.register_clone_type_with_name::<Arc<str>>(
            "String",
            "The string type",
        )
        .unwrap();

        /// Append a string to another, creating a new string
        ///
        /// ```roto
        /// "hello".append(" ").append("world") # -> "hello world"
        /// ```
        #[roto_method(rt, Arc<str>)]
        fn append(a: Arc<str>, b: Arc<str>) -> Arc<str> {
            format!("{a}{b}").into()
        }

        /// Check whether a string contains another string
        ///
        /// ```roto
        /// "haystack".contains("hay")  # -> true
        /// "haystack".contains("corn") # -> false
        /// ```
        #[roto_method(rt, Arc<str>)]
        fn contains(haystack: Arc<str>, needle: Arc<str>) -> bool {
            haystack.contains(needle.as_ref())
        }

        /// Check whether a string starts with a given prefix
        ///
        /// ```roto
        /// "haystack".contains("hay")   # -> true
        /// "haystack".contains("trees") # -> false
        /// ```
        #[roto_method(rt, Arc<str>)]
        fn starts_with(s: Arc<str>, prefix: Arc<str>) -> bool {
            s.starts_with(prefix.as_ref())
        }

        /// Check whether a string end with a given suffix
        ///
        /// ```roto
        /// "haystack".contains("stack") # -> true
        /// "haystack".contains("black") # -> false
        /// ```
        #[roto_method(rt, Arc<str>)]
        fn ends_with(s: Arc<str>, suffix: Arc<str>) -> bool {
            s.ends_with(suffix.as_ref())
        }

        /// Create a new string with all characters converted to lowercase
        ///
        /// ```roto
        /// "LOUD".to_lowercase() # -> "loud"
        /// ```
        #[roto_method(rt, Arc<str>)]
        fn to_lowercase(s: Arc<str>) -> Arc<str> {
            s.to_lowercase().into()
        }

        /// Create a new string with all characters converted to lowercase
        ///
        /// ```roto
        /// "quiet".to_uppercase() # -> "QUIET"
        /// ```
        #[roto_method(rt, Arc<str>)]
        fn to_uppercase(s: Arc<str>) -> Arc<str> {
            s.to_uppercase().into()
        }

        /// Repeat a string `n` times and join them
        ///
        /// ```roto
        /// "ha".repeat(6) # -> "hahahahahaha"
        /// ```
        #[roto_method(rt, Arc<str>)]
        fn repeat(s: Arc<str>, n: u32) -> Arc<str> {
            s.repeat(n as usize).into()
        }

        /// Check for string equality
        #[roto_method(rt, Arc<str>)]
        fn eq(s: Arc<str>, other: Arc<str>) -> bool {
            s == other
        }

        rt
    }

    // We might not use this, but let's keep it around for now (as of 27/8/2024)
    #[allow(unused)]
    fn find_type(&self, id: TypeId, name: &str) -> Result<&Ty, String> {
        match self.type_registry.get(id) {
            Some(t) => Ok(t),
            None => Err(format!(
                "Type `{name}` has not been registered and cannot be inspected by Roto"
            )),
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
        let mut rt = Runtime::new();

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
            "The well-known BLACKHOLE community.",
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
                "f32",
                "f64",
                "Asn",
                "IpAddr",
                "Prefix",
                "String",
                "OriginType",
                "LocalPref",
                "Community",
                "HopPath",
                "AsPath"
            ]
        );
    }
}
