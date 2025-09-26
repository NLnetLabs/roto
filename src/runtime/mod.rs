//! Runtime definition for Roto

mod basic;
pub mod context;
mod docs;
pub mod func;
mod io;
pub mod items;
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
use func::FunctionDescription;
use layout::Layout;
use ty::{Reflect, Ty, TypeDescription, TypeRegistry};

use crate::{
    ast::Identifier,
    file_tree::FileTree,
    parser::token::{Lexer, Token},
    runtime::{
        func::RegisterableFn,
        items::{Constant, Function, IntoItems, Item, Module, Type, Use},
    },
    typechecker::{
        scope::{ResolvedName, ScopeRef},
        TypeChecker,
    },
    Context, Package, RotoReport,
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
/// - [`Runtime::register_fn`]
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
    pub(crate) type_checker: TypeChecker,
    context: Option<ContextDescription>,
    types: Vec<RuntimeType>,
    functions: Vec<RuntimeFunction>,
    constants: HashMap<ResolvedName, RuntimeConstant>,
}

impl std::fmt::Debug for Runtime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Runtime").finish()
    }
}

impl Runtime {
    /// Compile a script from a path and return the result.
    ///
    /// If the path is a file, then that file will be loaded. If the path is a
    /// directory, the directory will be scanned for modules.
    pub fn compile(
        &self,
        path: impl AsRef<Path>,
    ) -> Result<Package, RotoReport> {
        FileTree::read(path)?.compile(self)
    }
}

/// Inspecting the [`Runtime`]
impl Runtime {
    /// A Runtime that is as empty as possible.
    ///
    /// This contains only type information for Roto primitives.
    pub fn new() -> Self {
        let mut this = Self {
            type_checker: TypeChecker::new(),
            context: None,
            types: Default::default(),
            functions: Default::default(),
            constants: Default::default(),
        };
        this.add_items(basic::built_ins()).unwrap();
        this
    }

    pub fn from_items(items: impl IntoItems) -> Result<Self, String> {
        let mut rt = Self::new();
        rt.add_items(items)?;
        Ok(rt)
    }

    fn add_items(&mut self, items: impl IntoItems) -> Result<(), String> {
        let root = ScopeRef::GLOBAL;
        let items = items.into_items();
        self.declare_modules(None, &items)?;
        self.declare_types(root, &items)?;
        self.declare_functions(root, &items)?;
        self.declare_constants(root, &items)?;
        self.declare_imports(root, &items)?;
        // rt.declare_context(root, &all_items)?;
        Ok(())
    }

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
    pub fn constants(&self) -> &HashMap<ResolvedName, RuntimeConstant> {
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
    name: ResolvedName,

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
    pub fn name(&self) -> ResolvedName {
        self.name.clone()
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
    pub(crate) name: ResolvedName,

    /// Description of the signature of the function
    pub(crate) func: FunctionDescription,

    /// Unique identifier for this function
    pub(crate) id: usize,

    /// Documentation for this function
    pub(crate) doc: String,

    /// Names of the parameters of this function for generated documentation
    pub(crate) params: Vec<Identifier>,
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
    pub name: ResolvedName,
    pub ty: TypeId,
    pub docstring: String,
    pub value: ConstantValue,
}

#[derive(Clone)]
pub struct ConstantValue(Arc<dyn Send + Sync + 'static>);

impl std::fmt::Debug for ConstantValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Constant")
            .field(&Arc::as_ptr(&self.0))
            .finish()
    }
}

impl ConstantValue {
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
    #[deprecated = "use items! and Runtime::add_items instead"]
    pub fn register_clone_type<T: Reflect + Clone>(
        &mut self,
        docstring: &str,
    ) -> Result<(), String> {
        let name = Self::extract_name::<T>();
        self.add_items(items::Type::clone::<T>(name, docstring)?)
    }

    /// Register a `Copy` type with a default name
    ///
    /// See [`Runtime::register_clone_type`]
    #[deprecated = "use items! and Runtime::add_items instead"]
    pub fn register_copy_type<T: Reflect + Copy>(
        &mut self,
        docstring: &str,
    ) -> Result<(), String> {
        let name = Self::extract_name::<T>();
        self.add_items(items::Type::copy::<T>(name, docstring)?)
    }

    #[deprecated = "use items! and Runtime::add_items instead"]
    pub fn register_copy_type_with_name<T: Reflect + Copy>(
        &mut self,
        name: &str,
        docstring: &str,
    ) -> Result<(), String> {
        self.add_items(items::Type::copy::<T>(name, docstring)?)
    }

    /// Register a reference type with a given name
    ///
    /// This makes the type available for use in Roto. However, Roto will
    /// only store pointers to this type.
    #[deprecated = "use items! and Runtime::add_items instead"]
    pub fn register_clone_type_with_name<T: Reflect + Clone>(
        &mut self,
        name: &str,
        docstring: &str,
    ) -> Result<(), String> {
        self.add_items(items::Type::clone::<T>(name, docstring)?)
    }

    #[deprecated = "use items! and Runtime::add_items instead"]
    pub fn register_fn<'a, A, R>(
        &mut self,
        name: impl AsRef<str>,
        docstring: impl AsRef<str>,
        argument_names: impl IntoIterator<Item = &'a str>,
        func: impl RegisterableFn<A, R>,
    ) -> Result<(), String> {
        let argument_names = argument_names.into_iter().collect();
        self.add_items(items::Function::new(
            name.as_ref(),
            docstring,
            argument_names,
            func,
        )?)
    }

    #[deprecated = "use items! and Runtime::add_items instead"]
    pub fn register_method<'a, T: Reflect, A, R>(
        &mut self,
        name: impl AsRef<str>,
        docstring: impl AsRef<str>,
        argument_names: impl IntoIterator<Item = &'a str>,
        func: impl RegisterableFn<A, R>,
    ) -> Result<(), String> {
        let argument_names = argument_names.into_iter().collect();
        let mut impl_block = items::Impl::new::<T>();
        impl_block.add(items::Function::new(
            name.as_ref(),
            docstring,
            argument_names,
            func,
        )?);
        self.add_items(impl_block)
    }

    #[deprecated = "use items! and Runtime::add_items instead"]
    pub fn register_static_method<'a, T: Reflect, A, R>(
        &mut self,
        name: impl AsRef<str>,
        docstring: impl AsRef<str>,
        argument_names: impl IntoIterator<Item = &'a str>,
        func: impl RegisterableFn<A, R>,
    ) -> Result<(), String> {
        #[allow(deprecated)]
        self.register_method::<T, _, _>(name, docstring, argument_names, func)
    }

    /// Register a new global constant
    ///
    /// Constants are shared between function functions. Since functions
    /// can be send to other threads, the constants must be `Send` and `Sync`.
    #[deprecated = "use items! and Runtime::add_items instead"]
    pub fn register_constant<T: Reflect>(
        &mut self,
        name: impl Into<String>,
        docstring: &str,
        x: T,
    ) -> Result<(), String>
    where
        T::Transformed: Send + Sync + 'static,
    {
        self.add_items(items::Constant::new::<T>(name.into(), docstring, x)?)
    }

    fn declare_modules(
        &mut self,
        scope: Option<ScopeRef>,
        items: &[Item],
    ) -> Result<(), String> {
        for item in items {
            match item {
                Item::Function(_) => {}
                Item::Type(_) => {}
                Item::Constant(_) => {}
                Item::Impl(_) => {}
                Item::Use(_) => {}
                Item::Module(module) => self.declare_module(scope, module)?,
            }
        }
        Ok(())
    }

    fn declare_module(
        &mut self,
        scope: Option<ScopeRef>,
        module: &Module,
    ) -> Result<(), String> {
        let scope = self
            .type_checker
            .declare_runtime_module(scope, module.ident)?;

        self.declare_modules(Some(scope), &module.children)?;

        Ok(())
    }

    fn declare_types(
        &mut self,
        scope: ScopeRef,
        items: &[Item],
    ) -> Result<(), String> {
        for item in items {
            match item {
                Item::Module(module) => {
                    let scope = self
                        .type_checker
                        .get_scope_of(scope, module.ident)
                        .unwrap();
                    self.declare_types(scope, &module.children)?;
                }
                Item::Type(ty) => self.declare_type(scope, ty)?,
                _ => {}
            }
        }
        Ok(())
    }

    fn declare_type(
        &mut self,
        scope: ScopeRef,
        ty: &Type,
    ) -> Result<(), String> {
        if let Some(old_ty) = self
            .types
            .iter()
            .find(|old_ty| old_ty.type_id == ty.type_id)
        {
            // TODO: Print scope in this error message
            return Err(format!(
                "Type {} is already registered under a different name: {}`",
                ty.rust_name, old_ty.name.ident,
            ));
        }

        self.type_checker
            .declare_runtime_type(scope, ty.ident, ty.type_id)?;

        let name = ResolvedName {
            scope,
            ident: ty.ident,
        };

        self.types.push(RuntimeType {
            name: name,
            type_id: ty.type_id,
            movability: ty.movability.clone(),
            layout: ty.layout.clone(),
            docstring: ty.doc.clone(),
        });

        Ok(())
    }

    fn declare_functions(
        &mut self,
        scope: ScopeRef,
        items: &[Item],
    ) -> Result<(), String> {
        for item in items {
            match item {
                Item::Module(Module {
                    ident, children, ..
                }) => {
                    let scope = self
                        .type_checker
                        .get_scope_of(scope, *ident)
                        .unwrap();
                    self.declare_functions(scope, children)?;
                }
                Item::Type(Type {
                    ident, children, ..
                }) => {
                    let scope = self
                        .type_checker
                        .get_scope_of(scope, *ident)
                        .unwrap();
                    self.declare_methods(scope, children)?;
                }
                Item::Function(f) => {
                    self.declare_function(scope, f, false)?;
                }
                Item::Impl(items::Impl { ty, children }) => {
                    let ty = self
                        .types
                        .iter()
                        .find(|t| t.type_id == *ty)
                        .ok_or("Type not found")?;
                    let scope = ty.name.scope;
                    let ident = ty.name.ident;
                    let scope =
                        self.type_checker.get_scope_of(scope, ident).unwrap();
                    self.declare_methods(scope, children)?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn declare_methods(
        &mut self,
        scope: ScopeRef,
        items: &[Item],
    ) -> Result<(), String> {
        for item in items {
            match item {
                Item::Function(f) => {
                    self.declare_function(scope, f, true)?;
                }
                Item::Impl(_) => {
                    return Err("Cannot nest an impl in an impl".into());
                }
                Item::Type(_) => {
                    return Err("Cannot nest a type in an impl".into());
                }
                Item::Module(_) => {
                    return Err("Cannot nest a module in a type".into());
                }
                Item::Use(_) => {}
                Item::Constant(_) => {}
            }
        }

        Ok(())
    }

    fn declare_function(
        &mut self,
        scope: ScopeRef,
        f: &Function,
        method: bool,
    ) -> Result<(), String> {
        Self::check_name(f.ident)?;

        let parameter_types: Vec<_> = f
            .func
            .parameter_types()
            .iter()
            .map(|ty| TypeChecker::rust_type_to_roto_type(self, *ty))
            .collect::<Result<_, _>>()?;

        let return_type =
            TypeChecker::rust_type_to_roto_type(self, f.func.return_type())?;

        let id = self.functions.len();
        let func = RuntimeFunction {
            name: ResolvedName {
                scope,
                ident: f.ident,
            },
            id,
            func: f.func.clone(),
            doc: f.doc.clone(),
            params: f.params.clone(),
        };
        self.functions.push(func);

        self.type_checker.declare_runtime_function(
            scope,
            f.ident,
            RuntimeFunctionRef(id),
            parameter_types,
            return_type,
            method,
        )?;

        Ok(())
    }

    fn declare_constants(
        &mut self,
        scope: ScopeRef,
        items: &[Item],
    ) -> Result<(), String> {
        for item in items {
            match item {
                Item::Module(module) => {
                    let scope = self
                        .type_checker
                        .get_scope_of(scope, module.ident)
                        .unwrap();
                    self.declare_types(scope, &module.children)?;
                }
                Item::Constant(c) => self.declare_constant(scope, c)?,
                _ => {}
            }
        }
        Ok(())
    }

    fn declare_constant(
        &mut self,
        scope: ScopeRef,
        constant: &Constant,
    ) -> Result<(), String> {
        let ty =
            TypeChecker::rust_type_to_roto_type(&self, constant.type_id)?;
        self.type_checker.declare_runtime_constant(
            scope,
            constant.ident,
            ty,
        )?;
        let name = ResolvedName {
            scope,
            ident: constant.ident,
        };
        self.constants.insert(
            name,
            RuntimeConstant {
                name: name,
                ty: constant.type_id,
                docstring: constant.doc.clone(),
                value: constant.value.clone(),
            },
        );
        Ok(())
    }

    fn declare_imports(
        &mut self,
        scope: ScopeRef,
        items: &[Item],
    ) -> Result<(), String> {
        for item in items {
            match item {
                Item::Function(_) => {}
                Item::Type(_) => {}
                Item::Constant(_) => {}
                Item::Impl(_) => {}
                Item::Use(use_item) => {
                    self.declare_import(scope, use_item)?
                }
                Item::Module(module) => {
                    self.declare_imports(scope, &module.children)?
                }
            }
        }
        Ok(())
    }

    fn declare_import(
        &mut self,
        scope: ScopeRef,
        use_item: &Use,
    ) -> Result<(), String> {
        for import in &use_item.imports {
            let mut new_scope = scope;
            let path = &import[..import.len() - 1];
            let last = &import[import.len() - 1];
            for part in path {
                new_scope = self
                    .type_checker
                    .get_scope_of(scope, part.into())
                    .ok_or("Could not get scope")?;
            }
            self.type_checker.declare_runtime_import(
                scope,
                ResolvedName {
                    scope: new_scope,
                    ident: last.into(),
                },
            )?;
        }
        Ok(())
    }

    fn extract_name<T: Reflect>() -> &'static str {
        let mut name = T::name();

        if let Some((first, _)) = name.split_once('<') {
            name = first;
        }
        if let Some((_, second)) = name.rsplit_once("::") {
            name = second;
        }
        name
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

    pub(crate) fn get_runtime_type(
        &self,
        id: TypeId,
    ) -> Option<&RuntimeType> {
        self.types.iter().find(|ty| ty.type_id == id)
    }

    /// Check that the given string is a valid Roto identifier
    fn check_name(name: Identifier) -> Result<(), String> {
        let mut lexer = Lexer::new(name.as_str());
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
