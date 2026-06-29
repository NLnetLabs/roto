//! Type definitions for the MIR
//!
//! The representation for types in the typechecker is not suitable for the MIR.
//! because we should only have _concrete_ types (i.e. no type variables) at
//! this point. Therefore, we can put them in a more efficient and ergonomic
//! representation.
//!
//! Another difference with the typechecker is that we do not really care about
//! the names of types here, instead we mostly care about the structure.
//! For example, this allows us to discard the name of a record type and just
//! represent it the same way that we would represent an anonymous record with
//! the same fields.
//!
//! First, we have [`Ty`], an enum describing all the kinds of types. We store
//! [`Ty`]s in a [`Pool`], which functions as an interner, giving out
//! [`TyRef`]s, which implement [`Copy`].

use indexmap::IndexSet;

use crate::{
    ast::Identifier,
    runtime::{
        Rt,
        layout::{Layout, LayoutBuilder},
    },
    typechecker::{
        info::TypeInfo,
        scoped_display::TypeDisplay,
        types::{FloatSize, IntKind, IntSize, Primitive},
    },
    value::ErasedList,
};

/// Types that the MIR refers to.
///
/// This represents only concrete, so there are no type variables in this
/// representation. All the types in a compilation should be stored in a
/// single [`Pool`].
///
/// Prefer storing [`TyRef`] over [`Ty`] as it is smaller and implements
/// [`Copy`].
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Ty {
    Unit,
    Never,
    Record(Vec<(Identifier, TyRef)>),
    Enum(Vec<(Identifier, Vec<TyRef>)>),
    Primitive(Primitive),
    List(TyRef),
    Runtime(std::any::TypeId),
}

/// A function signature in the MIR
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Signature {
    pub parameter_types: Vec<TyRef>,
    pub return_type: TyRef,
}

/// A reference to a [`Ty`] in a [`Pool`].
///
/// Many built-ins have a statically defined value which are available as constants.
///
/// If two [`TyRef`]s are created from the same [`Pool`], then they will compare
/// equal if and only if the underlying type is the same.
///
/// There are a couple of "well-known" [`TyRef`]s that always refer to the same
/// types for some of the primitives. These are available as constants on the
/// [`TyRef`] type.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TyRef(usize);

impl TyRef {
    pub fn type_id(self) -> usize {
        self.0
    }
}

impl TyRef {
    pub const UNIT: Self = Self(0);
    pub const NEVER: Self = Self(1);
    pub const BOOL: Self = Self(2);
    pub const U8: Self = Self(3);
    pub const U16: Self = Self(4);
    pub const U32: Self = Self(5);
    pub const U64: Self = Self(6);
    pub const I8: Self = Self(7);
    pub const I16: Self = Self(8);
    pub const I32: Self = Self(9);
    pub const I64: Self = Self(10);
    pub const F32: Self = Self(11);
    pub const F64: Self = Self(12);
    pub const STRING: Self = Self(13);
}

/// A pool of [`Ty`]s acting as an interner.
#[derive(Clone)]
pub struct Pool {
    types: IndexSet<Ty>,
}

impl Default for Pool {
    fn default() -> Self {
        Self::new()
    }
}

impl Pool {
    pub fn new() -> Self {
        let mut this = Self {
            types: IndexSet::new(),
        };

        // Insert all the well-known types so that they get the ID that we
        // want. To be sure that we're doing it right, we check that the IDs
        // that we create are the ones we create. Obviously, we don't want
        // this to actually fail in practice, but it should also break most
        // of the tests if this happens.

        let actual_val = this.intern(Ty::Unit);
        assert_eq!(TyRef::UNIT, actual_val);

        let actual_val = this.intern(Ty::Never);
        assert_eq!(TyRef::NEVER, actual_val);

        for (expected_val, primitive) in [
            (TyRef::BOOL, Primitive::Bool),
            (TyRef::U8, Primitive::Int(IntKind::Unsigned, IntSize::I8)),
            (TyRef::U16, Primitive::Int(IntKind::Unsigned, IntSize::I16)),
            (TyRef::U32, Primitive::Int(IntKind::Unsigned, IntSize::I32)),
            (TyRef::U64, Primitive::Int(IntKind::Unsigned, IntSize::I64)),
            (TyRef::I8, Primitive::Int(IntKind::Signed, IntSize::I8)),
            (TyRef::I16, Primitive::Int(IntKind::Signed, IntSize::I16)),
            (TyRef::I32, Primitive::Int(IntKind::Signed, IntSize::I32)),
            (TyRef::I64, Primitive::Int(IntKind::Signed, IntSize::I64)),
            (TyRef::F32, Primitive::Float(FloatSize::F32)),
            (TyRef::F64, Primitive::Float(FloatSize::F64)),
            (TyRef::STRING, Primitive::String),
        ] {
            let actual_val = this.intern(Ty::Primitive(primitive));
            assert_eq!(expected_val, actual_val);
        }
        this
    }

    /// Compute the layout of a Roto type
    ///
    /// The layout of Roto types match the C representation of Rust types,
    /// because we cannot rely on the Rust representation.
    ///
    /// The C representation is described in the [Rust reference].
    ///
    /// The general rules are as follows:
    ///
    ///  - The minimum layout of any type is a size of 0 and an alignment of 1
    ///  - Each primitive has a size and alignment equal to itself.
    ///  - Each composite type has the alignment of the most-aligned field in it.
    ///  - Fields are laid out in order, each padded to their alignment.
    ///  - The size **must** be a multiple of the alignment.
    ///
    /// For enums we use the `#[repr(C, u8)]` representation, because other the
    /// other representations are platform-specific. This means that the tag for
    /// enums is a `u8` and therefore 1 byte.
    ///
    /// To implement these rules, we rely on the [`Layout`] struct from the Rust
    /// standard library. This also allows to get the layout of some Rust types
    /// we rely on.
    ///
    /// This function returns `None` if the type is uninhabited.
    ///
    /// [Rust reference]: https://doc.rust-lang.org/reference/type-layout.html
    pub(crate) fn layout_of(&self, ty: TyRef, rt: &Rt) -> Option<Layout> {
        let layout = match self.get(ty) {
            Ty::Never => return None,
            Ty::Unit => Layout::new(0, 1),
            Ty::Primitive(primitive) => primitive.layout(),
            Ty::Runtime(type_id) => {
                rt.get_runtime_type(*type_id).unwrap().layout()
            }
            Ty::Record(fields) => {
                let mut builder = LayoutBuilder::new();
                for &(_, t) in fields {
                    builder.add(&self.layout_of(t, rt)?);
                }
                builder.finish()
            }
            Ty::Enum(variants) => {
                let mut layout = None;

                for (_, fields) in variants {
                    let mut builder = LayoutBuilder::new();
                    builder.add(&Layout::of::<u8>());

                    let builder =
                        fields.iter().try_fold(builder, |mut b, t| {
                            let layout = self.layout_of(*t, rt)?;
                            b.add(&layout);
                            Some(b)
                        });

                    // If the variant contains uninhabited fields, the
                    // entire variant is uninhabited, so we don't need
                    // to consider it.
                    let Some(builder) = builder else {
                        continue;
                    };

                    let variant_layout = builder.finish();

                    layout = Some(
                        layout.map_or(variant_layout.clone(), |l: Layout| {
                            l.union(&variant_layout)
                        }),
                    );
                }

                layout?
            }
            Ty::List(_) => Layout::of::<ErasedList>(),
        };

        Some(layout)
    }

    /// Whether or not the type is passed around by reference or by value
    ///
    /// Roto always has by-value semantics, but we still have types that we
    /// store in stack slots and then operate on by pointer. That is what
    /// we mean here with a reference type.
    ///
    /// Registered types, enums, records, ip addrs, prefixes and strings are all
    /// reference types. Integers, floats, booleans and AS numbers are not.
    ///
    /// Returns `None` if the type in uninhabited (e.g. `!`).
    pub(crate) fn is_reference_type(
        &self,
        ty: TyRef,
        rt: &Rt,
    ) -> Option<bool> {
        if self.layout_of(ty, rt)?.size() == 0 {
            return Some(false);
        }

        let res = match self.get(ty) {
            Ty::Never => return None,
            Ty::Record(_) => true,
            Ty::Enum(_) => true,
            Ty::Primitive(
                Primitive::String | Primitive::IpAddr | Primitive::Prefix,
            ) => true,
            Ty::List(_) => true,
            Ty::Runtime(_) => true,
            Ty::Unit => false,
            Ty::Primitive(
                Primitive::Int(..)
                | Primitive::Float(..)
                | Primitive::Bool
                | Primitive::Char
                | Primitive::Asn,
            ) => false,
        };

        Some(res)
    }

    pub fn intern(&mut self, ty: Ty) -> TyRef {
        TyRef(self.types.insert_full(ty).0)
    }

    pub fn get(&self, ty_ref: TyRef) -> &Ty {
        &self.types[ty_ref.0]
    }
}

impl TypeDisplay for TyRef {
    fn fmt(
        &self,
        type_info: &TypeInfo,
        f: &mut core::fmt::Formatter<'_>,
    ) -> core::fmt::Result {
        let ty = type_info.ty_pool.get(*self);
        ty.fmt(type_info, f)
    }
}

impl TypeDisplay for Ty {
    fn fmt(
        &self,
        type_info: &TypeInfo,
        f: &mut core::fmt::Formatter<'_>,
    ) -> core::fmt::Result {
        match self {
            Ty::Unit => f.write_str("()")?,
            Ty::Never => f.write_str("!")?,
            Ty::Record(fields) => {
                f.write_str("{")?;
                let mut first = true;
                for (field_name, field_ty) in fields {
                    if !first {
                        f.write_str(", ")?;
                    }
                    f.write_str(field_name.as_str())?;
                    f.write_str(": ")?;
                    field_ty.fmt(type_info, f)?;
                    first = false;
                }
                f.write_str("}")?;
            }
            Ty::Enum(variants) => {
                f.write_str("enum { ")?;
                let mut first_variant = true;
                for (identifier, ty_refs) in variants {
                    if !first_variant {
                        f.write_str(", ")?;
                    }
                    identifier.fmt(type_info, f)?;
                    f.write_str("(")?;
                    let mut first_field = true;
                    for field in ty_refs {
                        if !first_field {
                            f.write_str(", ")?;
                        }
                        field.fmt(type_info, f)?;
                        first_field = false;
                    }
                    f.write_str(")")?;
                    first_variant = false;
                }
                f.write_str(" }")?;
            }
            Ty::Primitive(primitive) => primitive.fmt(type_info, f)?,
            Ty::List(inner) => {
                f.write_str("List[")?;
                inner.fmt(type_info, f)?;
                f.write_str("]")?;
            }
            Ty::Runtime(type_id) => {
                write!(f, "runtime({:?})", type_id)?;
            }
        }
        Ok(())
    }
}
