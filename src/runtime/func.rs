use std::any::TypeId;

use crate::codegen::check::{RotoFunc, RustIrFunction};

use super::ty::TypeRegistry;

pub struct Func<F: RotoFunc> {
    wrapper: <F as RotoFunc>::RustWrapper,
    docstring: &'static str,
    argument_names: &'static [&'static str],
}

impl<F: RotoFunc> Func<F> {
    /// Construct a new [`Func`] to register to Roto.
    ///
    /// # Safety
    ///
    /// The `wrapper` argument must be a function that can be called safely
    /// from Roto.
    ///
    /// Use the [`roto_function`], [`roto_method`] and [`roto_static_method`]
    /// macros to be sure of that.
    pub unsafe fn new(
        wrapper: <F as RotoFunc>::RustWrapper,
        docstring: &'static str,
        argument_names: &'static [&'static str],
    ) -> Self {
        Self {
            wrapper,
            docstring,
            argument_names,
        }
    }

    pub(crate) fn docstring(&self) -> &'static str {
        self.docstring
    }

    pub(crate) fn argument_names(&self) -> &'static [&'static str] {
        self.argument_names
    }

    pub(crate) fn to_function_description(
        self,
        type_registry: &mut TypeRegistry,
    ) -> FunctionDescription {
        let parameter_types = F::parameter_types(type_registry);
        let return_type = F::return_type(type_registry);
        let pointer = F::ptr(&self.wrapper);
        let ir_function = F::ir_function(&self.wrapper);

        FunctionDescription {
            parameter_types,
            return_type,
            pointer,
            ir_function,
        }
    }
}

#[derive(Clone)]
pub struct FunctionDescription {
    parameter_types: Vec<TypeId>,
    return_type: TypeId,
    pointer: *const u8,
    ir_function: RustIrFunction,
}

// SAFETY: FunctionDescription is only not Send and Sync because of the function
// pointer, but that's fine because those are static. The only constructors for
// FunctionDescription are in this module, so we know that it's only instantiated
// with these pointers that are ok to send and sync.
unsafe impl Send for FunctionDescription {}
unsafe impl Sync for FunctionDescription {}

impl FunctionDescription {
    pub fn parameter_types(&self) -> &[TypeId] {
        &self.parameter_types
    }

    pub fn return_type(&self) -> TypeId {
        self.return_type
    }

    pub fn pointer(&self) -> *const u8 {
        self.pointer
    }

    pub fn ir_function(&self) -> RustIrFunction {
        self.ir_function.clone()
    }
}

impl PartialEq for FunctionDescription {
    fn eq(&self, other: &Self) -> bool {
        self.parameter_types == other.parameter_types
            && self.return_type == other.return_type
            && self.pointer == other.pointer
    }
}

impl Eq for FunctionDescription {}

impl std::fmt::Debug for FunctionDescription {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FunctionDescription")
            .field("parameter_types", &self.parameter_types)
            .field("return_type", &self.return_type)
            .field("pointer", &self.pointer)
            .field("wrapped", &"<function>")
            .finish()
    }
}
