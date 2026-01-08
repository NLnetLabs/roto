use std::any::TypeId;

use crate::value::TypeDescription;
use crate::{
    Location, Value,
    ast::Identifier,
    runtime::{
        CloneDrop, ConstantValue, Movability, RegistrationError, Rt,
        extern_clone, extern_drop,
        func::{FunctionDescription, RegisterableFn},
        layout::Layout,
    },
};

/// A registerable item
#[derive(Clone, Debug)]
pub enum Item {
    Function(Function),
    Type(Type),
    Module(Module),
    Constant(Constant),
    Impl(Impl),
    Use(Use),
}

/// Trait implemented by items that can be registered into the [`Runtime`].
///
/// In practice, implementors of this trait can be passed to [`Runtime::add`] and
/// [`Runtime::from_lib`].
///
/// [`Runtime`]: super::Runtime
/// [`Runtime::add`]: super::Runtime
/// [`Runtime::from_lib`]: super::Runtime
pub trait Registerable: Sized {
    /// Create a library containing this item.
    fn into_lib(self) -> Library {
        let mut lib = Library::new();
        self.add_to_lib(&mut lib);
        lib
    }

    /// Add this item to an existing library.
    fn add_to_lib(self, lib: &mut Library);
}

/// A collection of registerable items.
///
/// This type can be constructed manually via [`Library::new`] and
/// [`Library::add`] or via the [`library!`](crate::library) macro.
#[derive(Clone, Debug)]
pub struct Library {
    pub(crate) items: Vec<Item>,
}

impl Default for Library {
    fn default() -> Self {
        Self::new()
    }
}

impl Library {
    /// Create a new [`Library`].
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    /// Add an item to the [`Library`].
    pub fn add(&mut self, item: Item) {
        self.items.push(item);
    }
}

impl Registerable for Library {
    fn into_lib(self) -> Library {
        self
    }

    fn add_to_lib(self, lib: &mut Library) {
        lib.items.extend(self.items);
    }
}

impl From<Vec<Item>> for Library {
    fn from(value: Vec<Item>) -> Self {
        Self { items: value }
    }
}

impl Registerable for Item {
    fn add_to_lib(self, lib: &mut Library) {
        lib.add(self)
    }
}

impl<const N: usize, T: Registerable> Registerable for [T; N] {
    fn add_to_lib(self, lib: &mut Library) {
        for el in self {
            el.add_to_lib(lib);
        }
    }
}

impl<T: Registerable> Registerable for Vec<T> {
    fn add_to_lib(self, lib: &mut Library) {
        for el in self {
            el.add_to_lib(lib);
        }
    }
}

/// A module containing other items
///
/// Can be constructed with [`Module::new`].
#[derive(Clone, Debug)]
pub struct Module {
    pub(crate) ident: Identifier,
    pub(crate) doc: String,
    pub(crate) children: Vec<Item>,
    pub(crate) location: Location,
}

impl Module {
    /// Construct a new [`Module`].
    ///
    /// Items can be added to this module with [`Module::add`].
    ///
    /// The `name` must be a valid Rust identifier. The `doc` parameter is the
    /// docstring that will be displayed in the documentation that Roto can
    /// generate. The `location` is used for error reporting while registering
    /// this item. You should generally pass `roto::location!()` to get a correct
    /// value.
    pub fn new(
        name: impl Into<Identifier>,
        doc: impl AsRef<str>,
        location: Location,
    ) -> Result<Self, RegistrationError> {
        let name = name.into();
        Rt::check_name(&location, name)?;

        Ok(Self {
            ident: name,
            doc: doc.as_ref().to_string(),
            children: Vec::new(),
            location,
        })
    }

    pub fn add(&mut self, items: impl Registerable) {
        self.children.extend(items.into_lib().items)
    }
}

impl Registerable for Module {
    fn add_to_lib(self, lib: &mut Library) {
        lib.add(self.into())
    }
}

impl From<Module> for Item {
    fn from(value: Module) -> Self {
        Item::Module(value)
    }
}

/// A Roto type
///
/// This type will be cloned and dropped many times, so make sure to have
/// a cheap [`Clone`] and [`Drop`] implementations, for example an
/// [`Rc`](std::rc::Rc) or an [`Arc`](std::sync::Arc).
///
/// Use one of the [`Type::clone`] or [`Type::copy`] constructors to construct
/// this type. [`Type::copy`] will generally be more performant than
/// [`Type::clone`], so you should prefer that if the type implements [`Copy`].
#[derive(Clone, Debug)]
pub struct Type {
    pub(crate) ident: Identifier,
    pub(crate) rust_name: &'static str,
    pub(crate) doc: String,
    pub(crate) type_id: TypeId,
    pub(crate) layout: Layout,
    pub(crate) movability: Movability,
    pub(crate) location: Location,
}

impl Type {
    /// A type implementing `Clone`.
    ///
    /// The `name` must be a valid Rust identifier. The `doc` parameter is the
    /// docstring that will be displayed in the documentation that Roto can
    /// generate. The `location` is used for error reporting while registering
    /// this item. You should generally pass `roto::location!()` to get a correct
    /// value.
    ///
    /// ```rust
    /// use roto::{Type, Val, location};
    ///
    /// #[derive(Clone)]
    /// struct Foo(i32);
    ///
    /// Type::clone::<Val<Foo>>("Foo", "This is a foo!", location!()).unwrap();
    /// ```
    pub fn clone<T: Value + Clone>(
        name: impl Into<Identifier>,
        doc: impl AsRef<str>,
        location: Location,
    ) -> Result<Self, RegistrationError> {
        let movability = Movability::CloneDrop(CloneDrop {
            clone: extern_clone::<T> as _,
            drop: extern_drop::<T> as _,
        });
        Self::new::<T>(name, doc, movability, location)
    }

    /// A type implementing `Copy`.
    ///
    /// The `name` must be a valid Roto identifier. The `doc` parameter is the
    /// docstring that will be displayed in the documentation that Roto can
    /// generate. The `location` is used for error reporting while registering
    /// this item. You should generally pass [`roto::location!()`] to get a correct
    /// value.
    ///
    /// ```rust
    /// use roto::{Type, Val, location};
    ///
    /// #[derive(Clone, Copy)]
    /// struct Foo(i32);
    ///
    /// Type::copy::<Val<Foo>>("Foo", "This is a foo!", location!()).unwrap();
    /// ```
    pub fn copy<T: Value + Copy>(
        name: impl Into<Identifier>,
        doc: impl AsRef<str>,
        location: Location,
    ) -> Result<Self, RegistrationError> {
        Self::new::<T>(name, doc, Movability::Copy, location)
    }

    /// For internal use only, might lead to unexpected behaviour if used incorrectly
    pub(crate) fn value<T: Value + Copy>(
        name: impl Into<Identifier>,
        doc: impl AsRef<str>,
        location: Location,
    ) -> Result<Self, RegistrationError> {
        Self::new::<T>(name, doc, Movability::Value, location)
    }

    fn new<T: Value>(
        name: impl Into<Identifier>,
        doc: impl AsRef<str>,
        movability: Movability,
        location: Location,
    ) -> Result<Self, RegistrationError> {
        let name = name.into();
        Rt::check_name(&location, name)?;

        let ty = T::resolve();

        let is_allowed = match ty.description {
            TypeDescription::Leaf => true,
            TypeDescription::Val(_) => true,
            TypeDescription::Option(_) => false,
            TypeDescription::Verdict(_, _) => false,
            TypeDescription::List(_) => false,
        };

        if !is_allowed {
            return Err(RegistrationError {
                message: format!(
                    "Cannot register the type `{}`. Only `Val<T>` types can be registered",
                    ty.rust_name
                ),
                location,
            });
        }

        Ok(Self {
            ident: name,
            rust_name: std::any::type_name::<T>(),
            doc: doc.as_ref().into(),
            type_id: ty.type_id,
            layout: ty.layout,
            movability,
            location,
        })
    }
}

impl Registerable for Type {
    fn add_to_lib(self, lib: &mut Library) {
        lib.add(self.into())
    }
}

impl From<Type> for Item {
    fn from(value: Type) -> Self {
        Item::Type(value)
    }
}

/// A function that can be registered.
///
/// Can be constructed with `Function::new`.
#[derive(Clone, Debug)]
pub struct Function {
    pub(crate) ident: Identifier,
    pub(crate) doc: String,
    pub(crate) params: Vec<Identifier>,
    pub(crate) func: FunctionDescription,
    pub(crate) sig: Option<String>,
    pub(crate) location: Location,
    pub(crate) vtables: Vec<Identifier>,
}

impl Function {
    /// Construct a new [`Function`].
    ///
    /// The function to be registered is passed as `func` and must implement
    /// [`RegisterableFn`].
    ///
    /// The `name` must be a valid Roto identifier. The `doc` parameter is the
    /// docstring that will be displayed in the documentation that Roto can
    /// generate. With `params`, you can pass the names for each of the
    /// parameters of the function, this is also used for generating
    /// documentation. The `location` is used for error reporting while
    /// registering this item. You should generally pass [`roto::location!()`]
    /// to get a correct value.
    ///
    /// ```rust
    /// use roto::{Function, location};
    ///
    /// fn double(x: i32) -> i32 {
    ///     2 * x
    /// }
    ///
    /// Function::new(
    ///     "double",
    ///     "Double this value.",
    ///     vec!["x"],
    ///     double,
    ///     location!(),
    /// ).unwrap();
    /// ```
    pub fn new<A, R, O>(
        name: impl Into<Identifier>,
        doc: impl AsRef<str>,
        params: Vec<&str>,
        func: impl RegisterableFn<A, R, O>,
        location: Location,
    ) -> Result<Self, RegistrationError> {
        let name = name.into();
        Rt::check_name(&location, name)?;

        let func = FunctionDescription::of(func);

        Ok(Self {
            ident: name,
            doc: doc.as_ref().into(),
            params: params.into_iter().map(|p| p.into()).collect(),
            sig: None,
            func,
            vtables: Vec::new(),
            location,
        })
    }

    /// Create a new generic Roto function
    ///
    /// This is very dangerous, since the generic signature must match up with the given
    /// function. At the moment, there are not enough guard rails in place to expose this
    /// functionality to downstream users, so this function is kept private.
    pub(crate) unsafe fn new_generic<A, R, O>(
        name: impl Into<Identifier>,
        doc: impl AsRef<str>,
        params: Vec<&str>,
        func: impl RegisterableFn<A, R, O>,
        sig: &str,
        vtables: Vec<&str>,
        location: Location,
    ) -> Result<Self, RegistrationError> {
        let name = name.into();
        Rt::check_name(&location, name)?;

        let func = FunctionDescription::of(func);

        Ok(Self {
            ident: name,
            doc: doc.as_ref().into(),
            params: params.into_iter().map(|p| p.into()).collect(),
            sig: Some(sig.to_owned()),
            func,
            vtables: vtables.into_iter().map(|p| p.into()).collect(),
            location,
        })
    }
}

impl Registerable for Function {
    fn add_to_lib(self, lib: &mut Library) {
        lib.add(self.into())
    }
}

impl From<Function> for Item {
    fn from(value: Function) -> Self {
        Item::Function(value)
    }
}

/// A constant value
///
/// Can be constructed with [`Constant::new`]
#[derive(Clone, Debug)]
pub struct Constant {
    pub(crate) ident: Identifier,
    pub(crate) type_id: TypeId,
    pub(crate) doc: String,
    pub(crate) value: ConstantValue,
    pub(crate) location: Location,
}

impl Constant {
    /// Construct a new [`Constant`].
    ///
    /// The value to be registered is passed as `val` and must implement
    /// [`Value`], like any registerable type.
    ///
    /// The `name` must be a valid Roto identifier. The `doc` parameter is the
    /// docstring that will be displayed in the documentation that Roto can
    /// generate. The `location` is used for error reporting while
    /// registering this item. You should generally pass [`roto::location!()`]
    /// to get a correct value.
    ///
    /// ```rust
    /// use roto::{Constant, Val, location};
    ///
    /// Constant::new(
    ///     "PI",
    ///     "The value of pi as f32",
    ///     3.14f32,
    ///     location!(),
    /// ).unwrap();
    /// ```
    pub fn new<T: Value>(
        name: impl Into<Identifier>,
        doc: impl AsRef<str>,
        val: T,
        location: Location,
    ) -> Result<Self, RegistrationError>
    where
        T::Transformed: Send + Sync + 'static,
    {
        let name = name.into();
        Rt::check_name(&location, name)?;

        let ty = T::resolve();

        Ok(Self {
            ident: name,
            doc: doc.as_ref().into(),
            type_id: ty.type_id,
            value: ConstantValue::new(val.transform()),
            location,
        })
    }
}

impl Registerable for Constant {
    fn add_to_lib(self, lib: &mut Library) {
        lib.add(self.into())
    }
}

impl From<Constant> for Item {
    fn from(value: Constant) -> Self {
        Item::Constant(value)
    }
}

/// An impl block, which adds methods to a type.
///
/// Can be constructed with [`Impl::new`].
#[derive(Clone, Debug)]
pub struct Impl {
    pub(crate) ty: TypeId,
    pub(crate) children: Vec<Item>,
    pub(crate) location: Location,
}

impl Impl {
    /// Construct a new [`Impl`] block for a given type `T`.
    ///
    /// The `location` is used for error reporting while registering this item.
    /// You should generally pass [`roto::location!()`] to get a correct value.
    ///
    /// ```rust
    /// use roto::{Impl, location, library};
    ///
    /// let mut impl_u32 = Impl::new::<u32>(location!());
    /// impl_u32.add(library! { /* more items */ });
    /// ```
    pub fn new<T: Value>(location: Location) -> Self {
        let ty = T::resolve();
        Self {
            ty: ty.type_id,
            children: Vec::new(),
            location,
        }
    }

    /// Add more registerable items to this [`Impl`] block.
    pub fn add(&mut self, items: impl Registerable) {
        self.children.extend(items.into_lib().items)
    }
}

impl Registerable for Impl {
    fn add_to_lib(self, lib: &mut Library) {
        lib.add(self.into())
    }
}

impl From<Impl> for Item {
    fn from(value: Impl) -> Self {
        Item::Impl(value)
    }
}

/// A use item, representing an import of items.
///
/// Can be constructed with [`Use::new`].
#[derive(Clone, Debug)]
pub struct Use {
    pub(crate) imports: Vec<Vec<String>>,
    pub(crate) location: Location,
}

impl Use {
    /// Construct a new [`Use`].
    ///
    /// Each element of `imports` represents a path to an item to import.
    ///
    /// The `location` is used for error reporting while registering this item.
    /// You should generally pass [`roto::location!()`] to get a correct value.
    ///
    /// ```rust
    /// use roto::{Use, location};
    ///
    /// Use::new(
    ///     vec![
    ///         vec!["Verdict".into(), "Accept".into()],
    ///         vec!["Verdict".into(), "Reject".into()],
    ///     ],
    ///     location!(),
    /// );
    pub fn new(imports: Vec<Vec<String>>, location: Location) -> Self {
        Self { imports, location }
    }
}

impl Registerable for Use {
    fn add_to_lib(self, lib: &mut Library) {
        lib.add(self.into())
    }
}

impl From<Use> for Item {
    fn from(value: Use) -> Self {
        Item::Use(value)
    }
}
