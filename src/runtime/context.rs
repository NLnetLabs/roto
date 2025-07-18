use std::any::{type_name, TypeId};

/// The context or environment that a Roto script runs in
///
/// This declares a set of global variables that are initialized just before
/// the script is run.
pub unsafe trait Context: 'static {
    /// Return the fields of this struct, their offsets and their types
    fn fields() -> Vec<ContextField>;

    fn description() -> ContextDescription {
        ContextDescription {
            type_id: TypeId::of::<Self>(),
            type_name: type_name::<Self>(),
            fields: Self::fields(),
        }
    }
}

#[derive(Clone)]
pub struct ContextDescription {
    pub type_id: TypeId,
    pub type_name: &'static str,
    pub fields: Vec<ContextField>,
}

#[derive(Clone, Debug)]
pub struct ContextField {
    pub name: &'static str,
    pub offset: usize,
    pub type_name: &'static str,
    pub type_id: TypeId,
    pub docstring: String,
}

unsafe impl Context for () {
    fn fields() -> Vec<ContextField> {
        Vec::new()
    }
}
