use std::any::TypeId;

use crate::codegen::check::{RotoFunc, RustIrFunction};

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
    pub fn of<F: RotoFunc>(wrapper: &F::RustWrapper) -> Self {
        let parameter_types = F::parameter_types();
        let return_type = F::return_type();
        let pointer = F::ptr(&wrapper);
        let ir_function = F::ir_function(&wrapper);

        Self {
            parameter_types,
            return_type,
            pointer,
            ir_function,
        }
    }

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
