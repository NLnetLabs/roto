//! Runtime definition for Roto

mod basic;
pub mod context;
mod docs;
pub mod func;
mod io;
pub mod layout;
pub mod option;
pub mod ty;
pub mod val;
pub mod verdict;

#[cfg(test)]
pub mod tests;

use std::{
    any::TypeId, collections::HashMap, path::Path, ptr, slice, str, sync::Arc,
};

use context::ContextDescription;
use func::{Func, FunctionDescription};
use layout::Layout;
use ty::{Reflect, Ty, TypeDescription, TypeRegistry};

use crate::{
    ast::Identifier,
    codegen::check::RotoFunc,
    parser::token::{Lexer, Token},
    Compiled, Context, FileTree, RotoReport,
};

/// Provides the types and functions that Roto can access via FFI
///
/// Roto is an embedded language, therefore, it must be hooked up to the
/// outside world to do anything useful besides pure computation. This
/// connection is provided by the [`Runtime`], which exposes types, methods
/// and functions to Roto.
///
/// Roto can run in different [`Runtime`]s, depending on the situation.
/// Extending the default [`Runtime`] is the primary way to extend the
/// capabilities of Roto.
///
/// ## Compiling a script
///
/// - [`Runtime::compile`]
///
/// ## Registering Types
///
/// - [`Runtime::register_copy_type`]
/// - [`Runtime::register_clone_type`]
/// - [`Runtime::register_copy_type_with_name`]
/// - [`Runtime::register_clone_type_with_name`]
///
/// ## Registering functions and methods
///
/// - [`Runtime::register_function`]
/// - [`Runtime::register_method`]
/// - [`Runtime::register_static_method`]
///
/// ## Registering contstants
///
/// - [`Runtime::register_constant`]
///
/// ## Registering context type
///
/// - [`Runtime::register_context_type`]
///
/// ## Printing documentation
///
/// - [`Runtime::print_documentation`]
#[derive(Clone)]
pub struct Runtime {
    context: Option<ContextDescription>,
    types: Vec<RuntimeType>,
    functions: Vec<RuntimeFunction>,
    constants: HashMap<Identifier, RuntimeConstant>,
}

impl Runtime {
    pub fn compile(
        &self,
        path: impl AsRef<Path>,
    ) -> Result<Compiled, RotoReport> {
        FileTree::read(path).compile(self)
    }
}

/// Inspecting the [`Runtime`]
impl Runtime {
    /// Get the context type, if any.
    pub fn context(&self) -> &Option<ContextDescription> {
        &self.context
    }

    /// Get the registered types.
    pub fn types(&self) -> &[RuntimeType] {
        &self.types
    }

    /// Get the registered functions.
    pub fn functions(&self) -> &[RuntimeFunction] {
        &self.functions
    }

    /// Get the registered constants.
    pub fn constants(&self) -> &HashMap<Identifier, RuntimeConstant> {
        &self.constants
    }
}

#[derive(Clone, Debug)]
pub enum Movability {
    /// This type is passed by value, only available for built-in types.
    Value,

    /// This type can be copied without calling clone and drop.
    Copy,

    /// This type needs a clone and drop function.
    CloneDrop(CloneDrop),
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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
    pub(crate) name: String,

    /// Description of the signature of the function
    pub(crate) description: FunctionDescription,

    /// Whether it's a free function, method or a static method
    pub(crate) kind: FunctionKind,

    /// Unique identifier for this function
    pub(crate) id: usize,

    pub(crate) docstring: &'static str,

    pub(crate) argument_names: &'static [&'static str],
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RuntimeFunctionRef(usize);

impl std::fmt::Display for RuntimeFunctionRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl RuntimeFunction {
    pub fn get_ref(&self) -> RuntimeFunctionRef {
        RuntimeFunctionRef(self.id)
    }
}

#[derive(Clone, Debug)]
pub struct RuntimeConstant {
    pub name: Identifier,
    pub ty: TypeId,
    pub docstring: String,
    pub value: Constant,
}

#[derive(Clone)]
pub struct Constant(Arc<dyn Send + Sync + 'static>);

impl std::fmt::Debug for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Constant")
            .field(&Arc::as_ptr(&self.0))
            .finish()
    }
}

impl Constant {
    pub fn new<T: Send + Sync + 'static>(x: T) -> Self {
        Self(Arc::new(x))
    }

    pub fn ptr(&self) -> *const () {
        Arc::as_ptr(&self.0) as *const ()
    }
}

impl Runtime {
    pub(crate) fn get_function(
        &self,
        f: RuntimeFunctionRef,
    ) -> &RuntimeFunction {
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
        Self::check_name(name)?;

        if let Some(ty) = self.types.iter().find(|ty| ty.name == name) {
            let name = TypeRegistry::get(ty.type_id).unwrap().rust_name;
            return Err(format!(
                "Type with name {name} already registered.\n\
                Previously registered type: {}\n\
                Newly registered type: {}",
                name,
                std::any::type_name::<T>(),
            ));
        }

        if let Some(ty) =
            self.types.iter().find(|ty| ty.type_id == TypeId::of::<T>())
        {
            return Err(format!(
                "Type {} is already registered under a different name: {}`",
                std::any::type_name::<T>(),
                ty.name,
            ));
        }

        TypeRegistry::store::<T>(ty::TypeDescription::Leaf);
        self.types.push(RuntimeType {
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
        TypeRegistry::store::<Ctx>(TypeDescription::Leaf);

        // All fields in the context must be known because they'll
        // be accessible from Roto.
        for field in &description.fields {
            self.find_type(field.type_id, field.type_name)?;
        }

        self.context = Some(description);

        Ok(())
    }

    pub fn register_function<F: RotoFunc>(
        &mut self,
        name: impl Into<String>,
        f: Func<F>,
    ) -> Result<(), String> {
        self.register_function_internal(name.into(), FunctionKind::Free, f)
    }

    pub fn register_method<T: 'static, F: RotoFunc>(
        &mut self,
        name: impl Into<String>,
        f: Func<F>,
    ) -> Result<(), String> {
        let description = f.to_function_description();

        let Some(first) = description.parameter_types().first() else {
            return Err("a method must have at least one parameter".into());
        };

        // `to_function_description` already checks the validity of the types
        // so unwrap is ok.
        let ty = TypeRegistry::get(*first).unwrap();

        let type_id = match ty.description {
            TypeDescription::Leaf => ty.type_id,
            TypeDescription::Val(type_id) => type_id,
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

        let kind = FunctionKind::Method(std::any::TypeId::of::<T>());
        self.register_function_internal(name.into(), kind, f)
    }

    pub fn register_static_method<T: 'static, F: RotoFunc>(
        &mut self,
        name: impl Into<String>,
        f: Func<F>,
    ) -> Result<(), String> {
        let kind = FunctionKind::StaticMethod(std::any::TypeId::of::<T>());
        self.register_function_internal(name.into(), kind, f)
    }

    fn register_function_internal<F: RotoFunc>(
        &mut self,
        name: String,
        kind: FunctionKind,
        f: Func<F>,
    ) -> Result<(), String> {
        let docstring = f.docstring();
        let argument_names = f.argument_names();
        let description = f.to_function_description();

        Self::check_name(&name)?;

        let type_id = match kind {
            FunctionKind::Free => None,
            FunctionKind::Method(type_id)
            | FunctionKind::StaticMethod(type_id) => Some(type_id),
        };
        self.check_name_collision(type_id, &name)?;
        self.check_description(&description)?;

        let id = self.functions.len();
        self.functions.push(RuntimeFunction {
            name,
            description,
            kind,
            id,
            docstring,
            argument_names,
        });
        Ok(())
    }

    /// Register a new global constant
    ///
    /// Constants are shared between function functions. Since functions
    /// can be send to other threads, the constants must be `Send` and `Sync`.
    pub fn register_constant<T: Reflect>(
        &mut self,
        name: impl Into<String>,
        docstring: &str,
        x: T,
    ) -> Result<(), String>
    where
        T::Transformed: Send + Sync + 'static,
    {
        let ty = TypeRegistry::resolve::<T>();
        let id = ty.type_id;
        let name = name.into();

        Self::check_name(&name)?;
        Self::check_name_collision(&self, None, &name)?;

        self.get_runtime_type(id).ok_or_else(|| {
            let ty = TypeRegistry::get(id).unwrap();
            format!(
                "Registered a constant with an unregistered type: `{}`",
                ty.rust_name
            )
        })?;

        let symbol = Identifier::from(name);

        self.constants.insert(
            symbol,
            RuntimeConstant {
                name: symbol,
                ty: ty.type_id,
                docstring: docstring.into(),
                value: Constant::new(x.transform()),
            },
        );

        Ok(())
    }

    pub(crate) fn get_runtime_type(
        &self,
        id: TypeId,
    ) -> Option<&RuntimeType> {
        let ty = TypeRegistry::get(id)?;
        let id = match ty.description {
            TypeDescription::Leaf => id,
            TypeDescription::Val(id) => id,
            _ => panic!(),
        };
        self.types.iter().find(|ty| ty.type_id == id)
    }

    /// Check that the given string is a valid Roto identifier
    fn check_name(name: &str) -> Result<(), String> {
        let mut lexer = Lexer::new(name);
        let Some((Ok(tok), _)) = lexer.next() else {
            return Err(format!(
                "Name {name:?} is not a valid Roto identifier"
            ));
        };

        if lexer.next().is_some() {
            return Err(format!("Name {name:?} contains multiple tokens and is not a valid Roto identifier"));
        }

        match tok {
            Token::Ident(_) => Ok(()),
            Token::Keyword(_) => {
                Err(format!("Name {name:?} is a keyword in Roto and therefore not a valid identifier"))
            }
            _ => {
                Err(format!("Name {name:?} is not a valid Roto identifier."))
            }
        }
    }

    /// Check that there doesn't already exist a function with the same name
    ///
    /// The `type_id` refers to the id of the type that a method or static
    /// method is registered under.
    fn check_name_collision(
        &self,
        type_id: Option<TypeId>,
        name: &str,
    ) -> Result<(), String> {
        let kind_to_id = |kind: &FunctionKind| match kind {
            FunctionKind::Method(id) | FunctionKind::StaticMethod(id) => {
                Some(*id)
            }
            FunctionKind::Free => None,
        };

        for f in &self.functions {
            if kind_to_id(&f.kind) == type_id && f.name == name {
                if let Some(id) = type_id {
                    let ty = TypeRegistry::get(id).unwrap();
                    let type_name = ty.rust_name;
                    return Err(format!("Symbol `{name}` on type `{type_name}` is declared twice in runtime"));
                } else {
                    return Err(format!(
                        "Symbol `{name}` is declared twice in runtime"
                    ));
                }
            }
        }

        if type_id.is_none() {
            let symbol = Identifier::from(name);
            if self.constants.iter().any(|(c, _)| *c == symbol) {
                return Err(format!(
                    "Symbol `{name}` is declared twice in runtime"
                ));
            }
        }

        Ok(())
    }

    fn check_description(
        &self,
        description: &FunctionDescription,
    ) -> Result<(), String> {
        let check_type = |id: &TypeId| {
            self.get_runtime_type(*id).ok_or_else(|| {
                let ty = TypeRegistry::get(*id).unwrap();
                format!(
                    "Registered a function using an unregistered type: `{}`",
                    ty.rust_name
                )
            })
        };

        // TODO: This check needs to be done recursively
        for type_id in description.parameter_types() {
            let ty = TypeRegistry::get(*type_id).unwrap();
            let runtime_ty = check_type(type_id)?;
            if let Movability::Value = runtime_ty.movability {
                if !matches!(ty.description, TypeDescription::Leaf) {
                    let ty = TypeRegistry::get(ty.type_id).unwrap();
                    return Err(format!(
                        "Type `{}` should be passed by value. Try removing the `Val<T>` around `T`",
                        ty.rust_name,
                    ));
                }
            }
        }

        Ok(())
    }

    fn find_type(&self, id: TypeId, name: &str) -> Result<&Ty, String> {
        match TypeRegistry::get(id) {
            Some(t) => Ok(t),
            None => Err(format!(
                "Type `{name}` has not been registered and cannot be inspected by Roto"
            )),
        }
    }

    #[cfg(feature = "cli")]
    pub fn cli(&self) {
        crate::cli(self)
    }
}

impl Default for Runtime {
    fn default() -> Self {
        Self::new()
    }
}

pub unsafe extern "C" fn init_string(
    s: *mut Arc<str>,
    data: *mut u8,
    len: u32,
) {
    let slice = unsafe { slice::from_raw_parts(data, len as usize) };
    let str = unsafe { std::str::from_utf8_unchecked(slice) };
    let arc = Arc::<str>::from(str);
    unsafe { ptr::write(s, arc) };
}
