//! Runtime definition for Roto

mod basic;
pub mod context;
mod docs;
pub mod func;
mod io;
pub mod items;
pub mod layout;

#[cfg(test)]
pub mod tests;

use std::{
    any::TypeId, collections::HashMap, marker::PhantomData, path::Path, ptr,
    slice, str, sync::Arc,
};

use crate::value::{Ty, TypeDescription, TypeRegistry};
use context::ContextDescription;
use func::FunctionDescription;
use layout::Layout;
use sealed::sealed;

use crate::{
    Context, Impl, Location, Package, RotoReport,
    ast::Identifier,
    file_tree::FileTree,
    parser::token::{Lexer, Token},
    runtime::items::{
        Constant, Function, Item, Module, Registerable, Type, Use,
    },
    typechecker::{
        TypeChecker,
        scope::{ResolvedName, ScopeRef},
    },
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
/// ## Registering types, functions and constants.
///
/// - [`Runtime::add`]
///
/// ## Registering context type
///
/// - [`Runtime::register_context_type`]
///
/// ## Printing documentation
///
/// - [`Runtime::print_documentation`]
#[derive(Clone)]
pub struct Runtime<Ctx: OptCtx> {
    pub(crate) rt: Rt,
    _ctx: PhantomData<Ctx>,
}

/// Inner type of the runtime, without the type parameter
#[derive(Clone)]
pub(crate) struct Rt {
    pub(crate) type_checker: TypeChecker,
    context: Option<ContextDescription>,
    types: Vec<RuntimeType>,
    functions: Vec<RuntimeFunction>,
    constants: HashMap<ResolvedName, RuntimeConstant>,
}

impl<Ctx: OptCtx> std::fmt::Debug for Runtime<Ctx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Runtime").finish()
    }
}

/// This trait is _sealed_, meaning that it cannot be implemented by downstream
/// crates.
#[sealed]
pub trait OptCtx: 'static + Clone {
    type Ctx: Context;
    fn get_context(&mut self) -> &mut Self::Ctx;

    fn try_to_no_ctx() -> Option<NoCtx>;
}

#[derive(Clone)]
pub struct Ctx<T>(pub T);

#[sealed]
impl<T: Context> OptCtx for Ctx<T> {
    type Ctx = T;

    fn try_to_no_ctx() -> Option<NoCtx> {
        None
    }

    fn get_context(&mut self) -> &mut Self::Ctx {
        &mut self.0
    }
}

#[derive(Clone)]
pub struct NoCtx;

#[sealed]
impl OptCtx for NoCtx {
    type Ctx = Self;

    fn try_to_no_ctx() -> Option<NoCtx> {
        Some(NoCtx)
    }

    fn get_context(&mut self) -> &mut Self::Ctx {
        self
    }
}

/// Compiling a script
impl<Ctx: OptCtx> Runtime<Ctx> {
    /// Compile a script from a path and return the result.
    ///
    /// If the path is a file, then that file will be loaded. If the path is a
    /// directory, the directory will be scanned for modules.
    pub fn compile(
        &self,
        path: impl AsRef<Path>,
    ) -> Result<Package<Ctx>, RotoReport> {
        FileTree::read(path)?.compile(self)
    }
}

/// Creating a [`Runtime`]
impl Runtime<NoCtx> {
    /// A Runtime that is as empty as possible.
    ///
    /// This contains only type information for Roto primitives.
    pub fn new() -> Self {
        let mut rt = Rt {
            type_checker: TypeChecker::new(),
            context: None,
            types: Default::default(),
            functions: Default::default(),
            constants: Default::default(),
        };
        rt.add(basic::built_ins()).unwrap();
        Self {
            rt,
            _ctx: PhantomData,
        }
    }

    /// Create a new [`Runtime`] with a given library.
    ///
    /// This is nothing more than a convenience function around
    /// [`Runtime::new`] followed by [`Runtime::add`].
    pub fn from_lib(
        items: impl Registerable,
    ) -> Result<Self, RegistrationError> {
        let mut rt = Self::new();
        rt.add(items)?;
        Ok(rt)
    }

    pub fn with_context_type<C: Context + 'static>(
        mut self,
    ) -> Result<Runtime<Ctx<C>>, String> {
        self.rt.register_context_type::<C>()?;
        Ok(Runtime {
            rt: self.rt,
            _ctx: PhantomData,
        })
    }
}

/// Inspecting and modifying the [`Runtime`]
impl<C: OptCtx> Runtime<C> {
    /// Add a library of items to this [`Runtime`]
    ///
    /// Typically, one would use the [`library!`](crate::library) macro to
    /// register items. This would look something like this:
    ///
    /// ```rust
    /// use roto::{Runtime, library};
    ///
    /// let mut rt = Runtime::new();
    /// rt.add(library! {
    ///     /* define items here */
    /// }).unwrap();
    /// ```
    ///
    /// See the documentation on the `library!` macro for more information.
    ///
    /// However, it is -- with a bit of extra boilerplate -- also possible to
    /// register items without using the macro. You can directly register one
    /// of the item types:
    ///
    ///  - [`Module`]
    ///  - [`Type`]
    ///  - [`Function`]
    ///  - [`Constant`]
    ///  - [`Impl`]
    ///  - [`Use`]
    ///
    /// Or you can register an [`Item`] which combines all the above.
    /// Additionally, you can register collections of these types, such
    /// as `Vec<Item>` or `[Item; N]`.
    ///
    /// For example:
    ///
    /// ```rust
    /// use roto::{Runtime, Constant, Function, Impl, location, Item};
    ///
    /// let mut rt = Runtime::new();
    ///
    /// // Add a single constant
    /// rt.add(
    ///     Constant::new(
    ///         "U32_MAX",
    ///         "The maximum value of a `u32`.",
    ///         u32::MAX,
    ///         location!(),
    ///     ).unwrap()
    /// ).unwrap();
    ///
    /// // Add a constant and a method:
    ///
    /// let mut impl_block = Impl::new::<i32>(location!());
    /// impl_block.add(Function::new(
    ///     "max",
    ///     "The maximum value of an `i32`",
    ///     vec![],
    ///     || i32::MAX,
    ///         location!(),
    /// ).unwrap());
    ///
    /// rt.add([
    ///     Item::Impl(impl_block),
    ///     Item::Constant(Constant::new(
    ///         "I32_MAX",
    ///         "The maximum value of an `i32`",
    ///         i32::MAX,
    ///         location!(),
    ///     ).unwrap())
    /// ]).unwrap();
    /// ```
    ///
    /// See also [`Runtime::from_lib`], which combines [`Runtime::new`] and
    /// [`Runtime::add`] into a single function.
    pub fn add(
        &mut self,
        items: impl Registerable,
    ) -> Result<(), RegistrationError> {
        self.rt.add(items)
    }

    /// Get the context type, if any.
    pub fn context(&self) -> &Option<ContextDescription> {
        self.rt.context()
    }

    /// Get the registered types.
    pub fn types(&self) -> &[RuntimeType] {
        self.rt.types()
    }

    /// Get the registered functions.
    pub fn functions(&self) -> &[RuntimeFunction] {
        self.rt.functions()
    }

    /// Get the registered constants.
    pub fn constants(&self) -> &HashMap<ResolvedName, RuntimeConstant> {
        self.rt.constants()
    }

    #[cfg(feature = "cli")]
    pub fn cli(&self) {
        crate::cli(self)
    }

    pub fn try_without_ctx(self) -> Option<Runtime<NoCtx>> {
        C::try_to_no_ctx().map(|_| Runtime {
            rt: self.rt,
            _ctx: PhantomData,
        })
    }
}

impl Rt {
    pub fn add(
        &mut self,
        items: impl Registerable,
    ) -> Result<(), RegistrationError> {
        let root = ScopeRef::GLOBAL;
        let items = items.into_lib().items;
        self.declare_modules(None, &items)?;
        self.declare_types(root, &items)?;
        self.declare_functions(root, &items)?;
        self.declare_constants(root, &items)?;
        self.declare_imports(root, &items)?;
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
    _docstring: String,
}

impl RuntimeType {
    pub fn name(&self) -> ResolvedName {
        self.name
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

/// An error that arose while registering items from a library
#[derive(Clone)]
pub struct RegistrationError {
    message: String,
    location: Location,
}

impl std::fmt::Debug for RegistrationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!(
            "Registration Error:\n\t{}\n\tdefined at: {}",
            self.message, self.location
        ))
    }
}

impl std::fmt::Display for RegistrationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        format!(
            "Registration Error:\n\t{}\n\tdefined at: {}",
            self.message, self.location
        )
        .fmt(f)
    }
}

impl std::error::Error for RegistrationError {}

impl Rt {
    pub fn get_function(&self, f: RuntimeFunctionRef) -> &RuntimeFunction {
        &self.functions[f.0]
    }
}

impl Rt {
    fn declare_modules(
        &mut self,
        scope: Option<ScopeRef>,
        items: &[Item],
    ) -> Result<(), RegistrationError> {
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
    ) -> Result<(), RegistrationError> {
        let scope = self
            .type_checker
            .declare_runtime_module(scope, module.ident, module.doc.clone())
            .map_err(|e| RegistrationError {
                message: e,
                location: module.location.clone(),
            })?;

        self.declare_modules(Some(scope), &module.children)?;

        Ok(())
    }

    fn declare_types(
        &mut self,
        scope: ScopeRef,
        items: &[Item],
    ) -> Result<(), RegistrationError> {
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
    ) -> Result<(), RegistrationError> {
        if let Some(old_ty) = self
            .types
            .iter()
            .find(|old_ty| old_ty.type_id == ty.type_id)
        {
            // TODO: Print scope in this error message
            return Err(RegistrationError {
                message: format!(
                    "Type {} is already registered under a different name: {}`",
                    ty.rust_name, old_ty.name.ident,
                ),
                location: ty.location.clone(),
            });
        }

        self.type_checker
            .declare_runtime_type(scope, ty.ident, ty.type_id, ty.doc.clone())
            .map_err(|e| RegistrationError {
                message: e,
                location: ty.location.clone(),
            })?;

        let name = ResolvedName {
            scope,
            ident: ty.ident,
        };

        self.types.push(RuntimeType {
            name,
            type_id: ty.type_id,
            movability: ty.movability.clone(),
            layout: ty.layout.clone(),
            _docstring: ty.doc.clone(),
        });

        Ok(())
    }

    fn declare_functions(
        &mut self,
        scope: ScopeRef,
        items: &[Item],
    ) -> Result<(), RegistrationError> {
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
                Item::Function(f) => {
                    self.declare_function(scope, f, false)?;
                }
                Item::Impl(items::Impl {
                    ty,
                    children,
                    location,
                }) => {
                    let ty = self
                        .types
                        .iter()
                        .find(|t| t.type_id == *ty)
                        .ok_or_else(|| RegistrationError {
                            message: "Impl block with unregistered type"
                                .into(),
                            location: location.clone(),
                        })?;

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
    ) -> Result<(), RegistrationError> {
        for item in items {
            match item {
                Item::Function(f) => {
                    self.declare_function(scope, f, true)?;
                }
                Item::Impl(x) => {
                    return Err(RegistrationError {
                        message: "Cannot nest an impl in an impl".into(),
                        location: x.location.clone(),
                    });
                }
                Item::Type(x) => {
                    return Err(RegistrationError {
                        message: "Cannot nest a type in an impl".into(),
                        location: x.location.clone(),
                    });
                }
                Item::Module(x) => {
                    return Err(RegistrationError {
                        message: "Cannot nest a module in an impl".into(),
                        location: x.location.clone(),
                    });
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
    ) -> Result<(), RegistrationError> {
        Self::check_name(&f.location, f.ident)?;

        let parameter_types: Vec<_> = f
            .func
            .parameter_types()
            .iter()
            .map(|ty| self.rust_type_to_roto_type(&f.location, *ty))
            .collect::<Result<_, _>>()?;

        let return_type =
            self.rust_type_to_roto_type(&f.location, f.func.return_type())?;

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

        self.type_checker
            .declare_runtime_function(
                scope,
                f.ident,
                RuntimeFunctionRef(id),
                f.params.clone(),
                parameter_types,
                return_type,
                f.doc.clone(),
                method,
            )
            .map_err(|e| RegistrationError {
                message: e,
                location: f.location.clone(),
            })?;

        Ok(())
    }

    fn declare_constants(
        &mut self,
        scope: ScopeRef,
        items: &[Item],
    ) -> Result<(), RegistrationError> {
        for item in items {
            match item {
                Item::Module(module) => {
                    let scope = self
                        .type_checker
                        .get_scope_of(scope, module.ident)
                        .unwrap();
                    self.declare_constants(scope, &module.children)?;
                }
                Item::Impl(Impl {
                    ty,
                    children,
                    location,
                }) => {
                    let ty = self
                        .types
                        .iter()
                        .find(|t| t.type_id == *ty)
                        .ok_or_else(|| RegistrationError {
                            message: "Impl block with unregistered type"
                                .into(),
                            location: location.clone(),
                        })?;

                    let scope = ty.name.scope;
                    let ident = ty.name.ident;
                    let scope =
                        self.type_checker.get_scope_of(scope, ident).unwrap();
                    self.declare_constants(scope, children)?;
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
    ) -> Result<(), RegistrationError> {
        let ty = self
            .rust_type_to_roto_type(&constant.location, constant.type_id)?;

        self.type_checker
            .declare_runtime_constant(
                scope,
                constant.ident,
                ty,
                constant.doc.clone(),
            )
            .map_err(|e| RegistrationError {
                message: e,
                location: constant.location.clone(),
            })?;

        let name = ResolvedName {
            scope,
            ident: constant.ident,
        };

        self.constants.insert(
            name,
            RuntimeConstant {
                name,
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
    ) -> Result<(), RegistrationError> {
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
    ) -> Result<(), RegistrationError> {
        for import in &use_item.imports {
            let mut new_scope = scope;
            let path = &import[..import.len() - 1];
            let last = &import[import.len() - 1];
            for part in path {
                new_scope = self
                    .type_checker
                    .get_scope_of(scope, part.into())
                    .ok_or_else(|| RegistrationError {
                        message: format!("Could not get scope of {}", part),
                        location: use_item.location.clone(),
                    })?;
            }
            self.type_checker
                .declare_runtime_import(
                    scope,
                    ResolvedName {
                        scope: new_scope,
                        ident: last.into(),
                    },
                )
                .map_err(|e| RegistrationError {
                    message: e,
                    location: use_item.location.clone(),
                })?;
        }
        Ok(())
    }

    fn rust_type_to_roto_type(
        &self,
        location: &Location,
        type_id: TypeId,
    ) -> Result<crate::typechecker::types::Type, RegistrationError> {
        TypeChecker::rust_type_to_roto_type(self, type_id).map_err(|e| {
            RegistrationError {
                message: e,
                location: location.clone(),
            }
        })
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
    fn check_name(
        location: &Location,
        name: Identifier,
    ) -> Result<(), RegistrationError> {
        Self::check_name_internal(name).map_err(|e| RegistrationError {
            message: e,
            location: location.clone(),
        })
    }

    fn check_name_internal(name: Identifier) -> Result<(), String> {
        let mut lexer = Lexer::new(name.as_str());
        let Some((Ok(tok), _)) = lexer.next() else {
            return Err(format!(
                "Name {name:?} is not a valid Roto identifier"
            ));
        };

        if lexer.next().is_some() {
            return Err(format!(
                "Name {name:?} contains multiple tokens and is not a valid Roto identifier"
            ));
        }

        match tok {
            Token::Ident(_) => Ok(()),
            Token::Keyword(_) => Err(format!(
                "Name {name:?} is a keyword in Roto and therefore not a valid identifier"
            )),
            _ => {
                Err(format!("Name {name:?} is not a valid Roto identifier."))
            }
        }
    }

    fn find_type(&self, id: TypeId, name: &str) -> Result<&Ty, String> {
        match TypeRegistry::get(id) {
            Some(t) => Ok(t),
            None => Err(format!(
                "Type `{name}` has not been registered and cannot be inspected by Roto"
            )),
        }
    }
}

impl Default for Runtime<NoCtx> {
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
