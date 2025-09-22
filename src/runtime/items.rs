use std::any::TypeId;

use crate::{
    ast::Identifier,
    runtime::{
        extern_clone, extern_drop,
        func::{FunctionDescription, RegisterableFn},
        layout::Layout,
        ty::TypeDescription,
        CloneDrop, ConstantValue, Movability,
    },
    Reflect, Runtime,
};

#[derive(Clone, Debug)]
pub enum Item {
    Function(Function),
    Type(Type),
    Module(Module),
    Constant(Constant),
}

pub trait IntoItems {
    fn into_items(self) -> Vec<Item>;
}

impl IntoItems for Item {
    fn into_items(self) -> Vec<Item> {
        vec![self]
    }
}

impl<const N: usize, T: IntoItems> IntoItems for [T; N] {
    fn into_items(self) -> Vec<Item> {
        let mut v = Vec::new();
        for el in self {
            v.extend(el.into_items())
        }
        v
    }
}

impl<T: IntoItems> IntoItems for Vec<T> {
    fn into_items(self) -> Vec<Item> {
        let mut v = Vec::new();
        for el in self {
            v.extend(el.into_items())
        }
        v
    }
}

#[derive(Clone, Debug)]
pub struct Module {
    pub(crate) ident: Identifier,
    pub(crate) doc: String,
    pub(crate) children: Vec<Item>,
}

impl Module {
    pub fn new(
        name: impl Into<Identifier>,
        doc: impl AsRef<str>,
    ) -> Result<Self, String> {
        let name = name.into();
        Runtime::check_name(name)?;

        Ok(Self {
            ident: name,
            doc: doc.as_ref().to_string(),
            children: Vec::new(),
        })
    }

    pub fn add(&mut self, items: impl IntoItems) {
        self.children.extend(items.into_items())
    }
}

impl IntoItems for Module {
    fn into_items(self) -> Vec<Item> {
        vec![self.into()]
    }
}

impl From<Module> for Item {
    fn from(value: Module) -> Self {
        Item::Module(value)
    }
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
#[derive(Clone, Debug)]
pub struct Type {
    pub(crate) ident: Identifier,
    pub(crate) rust_name: &'static str,
    pub(crate) doc: String,
    pub(crate) description: TypeDescription,
    pub(crate) type_id: TypeId,
    pub(crate) layout: Layout,
    pub(crate) movability: Movability,
    pub(crate) children: Vec<Item>,
}

impl Type {
    pub fn clone<T: Reflect + Clone>(
        name: impl Into<Identifier>,
        doc: impl AsRef<str>,
    ) -> Result<Self, String> {
        let movability = Movability::CloneDrop(CloneDrop {
            clone: extern_clone::<T> as _,
            drop: extern_drop::<T> as _,
        });
        Self::new::<T>(name, doc, movability)
    }

    pub fn copy<T: Reflect + Copy>(
        name: impl Into<Identifier>,
        doc: impl AsRef<str>,
    ) -> Result<Self, String> {
        Self::new::<T>(name, doc, Movability::Copy)
    }

    /// For internal use only, might lead to unexpected behaviour if used incorrectly
    pub(crate) fn value<T: Reflect + Copy>(
        name: impl Into<Identifier>,
        doc: impl AsRef<str>,
    ) -> Result<Self, String> {
        Self::new::<T>(name, doc, Movability::Value)
    }

    fn new<T: Reflect>(
        name: impl Into<Identifier>,
        doc: impl AsRef<str>,
        movability: Movability,
    ) -> Result<Self, String> {
        let name = name.into();
        Runtime::check_name(name)?;

        let ty = T::resolve();

        let is_allowed = match ty.description {
            TypeDescription::Leaf => true,
            TypeDescription::Val(_) => true,
            TypeDescription::Option(_) => false,
            TypeDescription::Verdict(_, _) => false,
        };

        if !is_allowed {
            return Err(format!(
                "Cannot register the type `{}`. Only `Val<T>` types can be registered",
                ty.rust_name
            ));
        }

        Ok(Self {
            ident: name,
            rust_name: std::any::type_name::<T>(),
            doc: doc.as_ref().into(),
            description: ty.description,
            type_id: ty.type_id,
            layout: ty.layout,
            movability,
            children: Vec::new(),
        })
    }

    pub fn add(&mut self, items: impl IntoItems) {
        self.children.extend(items.into_items())
    }
}

impl IntoItems for Type {
    fn into_items(self) -> Vec<Item> {
        vec![self.into()]
    }
}

impl From<Type> for Item {
    fn from(value: Type) -> Self {
        Item::Type(value)
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub(crate) ident: Identifier,
    pub(crate) doc: String,
    pub(crate) params: Vec<Identifier>,
    pub(crate) func: FunctionDescription,
}

impl Function {
    pub fn new<A, R>(
        name: impl Into<Identifier>,
        doc: impl AsRef<str>,
        params: Vec<impl Into<Identifier>>,
        func: impl RegisterableFn<A, R>,
    ) -> Result<Self, String> {
        let name = name.into();
        Runtime::check_name(name)?;

        let func = FunctionDescription::of(func);

        Ok(Self {
            ident: name,
            doc: doc.as_ref().into(),
            params: params.into_iter().map(|p| p.into()).collect(),
            func,
        })
    }
}

impl IntoItems for Function {
    fn into_items(self) -> Vec<Item> {
        vec![self.into()]
    }
}

impl From<Function> for Item {
    fn from(value: Function) -> Self {
        Item::Function(value)
    }
}

#[derive(Clone, Debug)]
pub struct Constant {
    pub(crate) ident: Identifier,
    pub(crate) type_id: TypeId,
    pub(crate) doc: String,
    pub(crate) value: ConstantValue,
}

impl Constant {
    pub fn new<T: Reflect>(
        name: impl Into<Identifier>,
        doc: impl AsRef<str>,
        val: T,
    ) -> Result<Self, String>
    where
        T::Transformed: Send + Sync + 'static,
    {
        let name = name.into();
        Runtime::check_name(name)?;

        let ty = T::resolve();

        Ok(Self {
            ident: name,
            doc: doc.as_ref().into(),
            type_id: ty.type_id,
            value: ConstantValue::new(val.transform()),
        })
    }
}

impl IntoItems for Constant {
    fn into_items(self) -> Vec<Item> {
        vec![self.into()]
    }
}

impl From<Constant> for Item {
    fn from(value: Constant) -> Self {
        Item::Constant(value)
    }
}
