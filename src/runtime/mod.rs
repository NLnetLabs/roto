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
pub mod wrap;

use std::{
    any::{Any, TypeId},
    mem::{align_of, size_of},
    net::IpAddr,
};

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

use self::wrap::WrappedFunction;
use crate::typechecker::types::{Primitive, Type};
use crate::wrap;

/// Provides the types and functions that Roto can access via FFI
///
/// Even some types that can be written as literals should be provided here.
/// The idea here is that Roto can be used with different representations
/// of these types in different applications. The type checker will yield an
/// error if a literal is provided for an undeclared type.
///
/// The primitive types are _not_ part of the runtime. That is, types like
/// booleans, integers and the unit type.
pub struct Runtime {
    pub types: Vec<RuntimeType>,
    // functions: RuntimeFunction,
}

pub struct RuntimeType {
    pub name: &'static str,
    pub representation: Representation,
    pub alignment: usize,
    pub size: usize,
    pub type_id: TypeId,
    pub methods: Vec<RuntimeMethod>,
    pub eq: Option<WrappedFunction>,
}

pub struct RuntimeMethod {
    pub name: &'static str,
    pub parameter_types: Vec<Type>,
    pub return_type: Type,
    pub wrapped: WrappedFunction,
}

impl Runtime {
    /// Register a type and its method for use in Roto
    ///
    /// The type parameter is the type to register. Among other things,
    /// the type determines the size of the type in Roto. The `name`
    /// parameter specifies how the type can be referred to in Roto.
    fn register_type<T: Any>(
        &mut self,
        name: &'static str,
        representation: Representation,
        eq: Option<WrappedFunction>,
        methods: Vec<RuntimeMethod>,
    ) {
        if self.types.iter().any(|ty| ty.name == name) {
            panic!("Type with name {name} already registered.")
        }
        if self.types.iter().any(|ty| ty.type_id == TypeId::of::<T>()) {
            panic!(
                "Type {} is already registered under a different name.`",
                std::any::type_name::<T>()
            )
        }
        self.types.push(RuntimeType {
            name,
            representation,
            size: size_of::<T>(),
            alignment: align_of::<T>(),
            type_id: TypeId::of::<T>(),
            methods,
            eq,
        })
    }

    pub fn get_type(&self, id: TypeId) -> &RuntimeType {
        let ty = self.types.iter().find(|ty| ty.type_id == id).unwrap();
        ty
    }
}

fn ip_address_eq(a: &IpAddr, b: &IpAddr) -> bool {
    a == b
}

fn prefix_eq(a: &Prefix, b: &Prefix) -> bool {
    a == b
}

fn prefix_addr(x: &Prefix) -> IpAddr {
    x.addr()
}

fn prefix_len(x: Prefix) -> u8 {
    x.len()
}

fn prefix_contains(x: Prefix, y: IpAddr) -> bool {
    x.contains(y)
}

fn prefix_covers(x: Prefix, y: Prefix) -> bool {
    x.covers(y)
}

fn as_path_origin(x: &AsPath<Vec<u8>>) -> u32 {
    // Good evidence for adding better error handling to roto...
    x.origin().unwrap().try_into_asn().unwrap().into_u32()
}

fn hop_path_origin(x: &HopPath) -> u32 {
    // Good evidence for adding better error handling to roto...
    x.origin()
        .unwrap()
        .clone()
        .try_into_asn()
        .unwrap()
        .into_u32()
}

fn hop_path_hop_count(x: &HopPath) -> u32 {
    x.hop_count() as u32
}

impl Runtime {
    fn empty() -> Self {
        Runtime {
            types: Default::default(),
        }
    }
}

pub enum Representation {
    Value,
    Reference,
}

impl Default for Runtime {
    fn default() -> Self {
        let mut rt = Self::empty();

        rt.register_type::<IpAddr>(
            "IpAddress",
            Representation::Value,
            Some(wrap!(ip_address_eq(&a, &b))),
            vec![],
        );

        rt.register_type::<OriginType>(
            "OriginType",
            Representation::Reference,
            None,
            vec![],
        );

        rt.register_type::<NextHop>(
            "NextHop",
            Representation::Reference,
            None,
            vec![],
        );

        rt.register_type::<MultiExitDisc>(
            "MultiExitDisc",
            Representation::Reference,
            None,
            vec![],
        );

        rt.register_type::<LocalPref>(
            "LocalPref",
            Representation::Reference,
            None,
            vec![],
        );

        rt.register_type::<Aggregator>(
            "Aggregator",
            Representation::Reference,
            None,
            vec![],
        );

        rt.register_type::<AtomicAggregate>(
            "AtomicAggregate",
            Representation::Reference,
            None,
            vec![],
        );

        rt.register_type::<Community>(
            "Community",
            Representation::Reference,
            None,
            vec![],
        );

        rt.register_type::<Prefix>(
            "Prefix",
            Representation::Value,
            Some(wrap!(prefix_eq(&a, &b))),
            vec![
                RuntimeMethod {
                    name: "address",
                    parameter_types: Vec::new(),
                    return_type: Type::Name("IpAddress".into()),
                    wrapped: wrap!(prefix_addr(&p)),
                },
                RuntimeMethod {
                    name: "len",
                    parameter_types: Vec::new(),
                    return_type: Type::Primitive(Primitive::U8),
                    wrapped: wrap!(prefix_len(*p)),
                },
                RuntimeMethod {
                    name: "contains",
                    parameter_types: vec![Type::Name("IpAddress".into())],
                    return_type: Type::Primitive(Primitive::Bool),
                    wrapped: wrap!(prefix_contains(*p, *a)),
                },
                RuntimeMethod {
                    name: "covers",
                    parameter_types: vec![Type::Name("Prefix".into())],
                    return_type: Type::Primitive(Primitive::Bool),
                    wrapped: wrap!(prefix_covers(*p, *p2)),
                },
            ],
        );

        rt.register_type::<HopPath>(
            "HopPath",
            Representation::Reference,
            None,
            vec![
                RuntimeMethod {
                    name: "origin",
                    parameter_types: Vec::new(),
                    return_type: Type::Name("AsNumber".into()),
                    wrapped: wrap!(hop_path_origin(&p)),
                },
                RuntimeMethod {
                    name: "hop_count",
                    parameter_types: Vec::new(),
                    return_type: Type::Primitive(Primitive::U32),
                    wrapped: wrap!(hop_path_hop_count(&p)),
                },
            ],
        );

        rt.register_type::<AsPath<Vec<u8>>>(
            "AsPath",
            Representation::Reference,
            None,
            vec![RuntimeMethod {
                name: "origin",
                parameter_types: Vec::new(),
                return_type: Type::Name("AsNumber".into()),
                wrapped: wrap!(as_path_origin(&p)),
            }],
        );

        rt
    }
}
