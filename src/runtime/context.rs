//! Declaring the context of Roto scripts

use std::any::{TypeId, type_name};

use sealed::sealed;

/// The context or environment that a Roto script runs in
///
/// This declares a set of global variables that are initialized just before
/// the script is run.
///
/// # Safety
///
/// This trait is unsafe because it tell Roto about the offsets and types of
/// each field. It is crucial that this information is correct. Therefore,
/// you should only ever derive this trait and not implement it manually.
pub unsafe trait Context: 'static + Clone {
    /// Return the fields of this struct, their offsets and their types
    fn fields() -> Vec<ContextField>;

    /// Return the [`ContextDescription`] of this [`Context`] type.
    fn description() -> ContextDescription {
        ContextDescription {
            type_id: TypeId::of::<Self>(),
            type_name: type_name::<Self>(),
            fields: Self::fields(),
        }
    }
}

/// This trait is _sealed_, meaning that it cannot be implemented by downstream
/// crates.
#[sealed]
pub trait OptCtx: 'static + Clone {
    /// The context type this represents.
    type Ctx: Context;

    /// Get the underlying context
    fn get_context(&mut self) -> &mut Self::Ctx;

    /// Try to convert this onnto `NoCtx`.
    fn try_to_no_ctx() -> Option<NoCtx>;
}

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

/// A type indicating that no context type is being used.
///
/// This is one of the two types implementing `OptCtx`.
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

/// A type indicating that a context type `T` is being used.
///
/// This is one of the two types implementing `OptCtx`.
#[derive(Clone)]
pub struct Ctx<T>(pub T);

/// Description of `Context` type
#[derive(Clone)]
pub struct ContextDescription {
    /// [`TypeId`] of this type.
    pub type_id: TypeId,

    /// Name of this type.
    pub type_name: &'static str,

    /// Fields of this type.
    pub fields: Vec<ContextField>,
}

/// Description of a field of a [`Context`] type
#[derive(Clone, Debug)]
pub struct ContextField {
    /// Name of the field
    pub name: &'static str,

    /// Offset of the field in its enclosing type
    pub offset: usize,

    /// Name of the type of the field
    pub type_name: &'static str,

    /// [`TypeId`] of the type of the field
    pub type_id: TypeId,

    /// Docstring of the field
    pub docstring: String,
}

unsafe impl Context for NoCtx {
    fn fields() -> Vec<ContextField> {
        Vec::new()
    }
}
