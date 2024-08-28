use std::collections::HashMap;

use crate::{ast::Identifier, parser::meta::MetaId};

use super::{
    scope::{DefinitionRef, ScopeRef}, types::{Function, Type}, unionfind::UnionFind
};

/// The output of the type checker that is used for lowering
#[derive(Clone)]
pub struct TypeInfo {
    /// The unionfind structure that maps type variables to types
    pub(super) unionfind: UnionFind,

    /// Map from type names to types
    pub(super) types: HashMap<Identifier, Type>,

    /// The types we inferred for each Expr
    ///
    /// This might not be fully resolved yet.
    pub(super) expr_types: HashMap<MetaId, Type>,

    /// The fully qualified (and hence unique) name for each identifier.
    pub(super) resolved_names: HashMap<MetaId, DefinitionRef>,

    /// Scopes of functions
    pub(super) function_scopes: HashMap<MetaId, ScopeRef>,

    /// The function that is called on each function call
    pub(super) function_calls: HashMap<MetaId, Function>,

    /// The ids of all the `Expr::Access` nodes that should be interpreted
    /// as enum variant constructors.
    pub(super) enum_variant_constructors: HashMap<MetaId, Type>,

    pub(super) diverges: HashMap<MetaId, bool>,

    /// Type for return/accept/reject that it constructs and returns.
    pub(super) return_types: HashMap<MetaId, Type>,

    // Size of the pointer type in bytes
    pointer_bytes: u32,
}

impl TypeInfo {
    pub fn new(pointer_bytes: u32) -> Self {
        Self {
            unionfind: UnionFind::default(),
            types: HashMap::new(),
            expr_types: HashMap::new(),
            resolved_names: HashMap::new(),
            enum_variant_constructors: HashMap::new(),
            diverges: HashMap::new(),
            return_types: HashMap::new(),
            function_calls: HashMap::new(),
            function_scopes: HashMap::new(),
            pointer_bytes,
        }
    }

    pub fn add_type(&mut self, name: Identifier, ty: Type) {
        if self.types.insert(name, ty).is_some() {
            panic!("Type was added to TypeInfo twice");
        }
    }
}

impl TypeInfo {
    pub fn resolved_name(&self, x: impl Into<MetaId>) -> DefinitionRef {
        self.resolved_names[&x.into()]
    }

    pub fn type_of(&mut self, x: impl Into<MetaId>) -> Type {
        let ty = self.expr_types[&x.into()].clone();
        self.resolve(&ty)
    }

    pub fn diverges(&mut self, x: impl Into<MetaId>) -> bool {
        self.diverges[&x.into()]
    }

    pub fn return_type_of(&mut self, x: impl Into<MetaId>) -> Type {
        let ty = self.return_types[&x.into()].clone();
        self.resolve(&ty)
    }

    pub fn function(&self, x: impl Into<MetaId>) -> &Function {
        self.function_calls.get(&x.into()).unwrap()
    }

    pub fn function_scope(&self, x: impl Into<MetaId>) -> ScopeRef {
        self.function_scopes[&x.into()]
    }

    pub fn offset_of(
        &mut self,
        record: &Type,
        field: Identifier,
    ) -> (Type, u32) {
        let record = self.resolve(record);

        let (Type::Record(fields)
        | Type::RecordVar(_, fields)
        | Type::NamedRecord(_, fields)) = record
        else {
            panic!("Can't get offsets in a type that's not a record")
        };

        let mut offset = 0;
        for (name, ty) in fields {
            // Here, we align the offset to the natural alignment of each
            // type.
            offset += self.padding_of(&ty, offset);
            if name.node == field {
                return (ty, offset);
            }
            offset += self.size_of(&ty);
        }
        panic!("Field not found")
    }

    pub fn padding_of(&mut self, ty: &Type, offset: u32) -> u32 {
        let alignment = self.alignment_of(ty);
        if offset % alignment > 0 {
            alignment - (offset % alignment)
        } else {
            0
        }
    }

    pub fn alignment_of(&mut self, ty: &Type) -> u32 {
        let align = match self.resolve(ty) {
            Type::RecordVar(_, fields)
            | Type::Record(fields)
            | Type::NamedRecord(_, fields) => fields
                .iter()
                .map(|f| self.alignment_of(&f.1))
                .max()
                .unwrap_or(1),
            Type::Enum(_, variants) => variants
                .iter()
                .flat_map(|(_, opt)| opt)
                .map(|f| self.alignment_of(f))
                .max()
                .unwrap_or(4),
            ty => self.size_of(&ty),
        };
        // Alignment must be guaranteed to be at least 1
        align.max(1)
    }

    pub fn enum_variant_constructor(
        &self,
        x: impl Into<MetaId>,
    ) -> Option<&Type> {
        self.enum_variant_constructors.get(&x.into())
    }

    pub fn resolve(&mut self, t: &Type) -> Type {
        let mut t = t.clone();

        if let Type::Var(x) | Type::IntVar(x) | Type::RecordVar(x, _) = t {
            t = self.unionfind.find(x).clone();
        }

        if let Type::Name(x) = t {
            t = self.types[&x].clone();
        }

        t
    }

    pub fn size_of(&mut self, t: &Type) -> u32 {
        let t = self.resolve(t);
        match t {
            // Never is zero-sized
            Type::Never => 0,
            // Int variables are inferred to u32
            Type::IntVar(_) => 4,
            // Records have the size of their fields
            Type::Record(fields)
            | Type::NamedRecord(_, fields)
            | Type::RecordVar(_, fields) => {
                let mut size = 0;
                for (_, ty) in &fields {
                    size += self.size_of(ty) + self.padding_of(ty, size);
                }
                size
            }
            Type::Enum(_, fields) => {
                fields
                    .iter()
                    .flat_map(|f| &f.1)
                    .map(|ty| self.size_of(ty) + self.padding_of(ty, 1))
                    .max()
                    .unwrap_or(0)
                    + 1 // add the discriminant
            }
            Type::Verdict(accept, reject) => {
                let accept =
                    self.size_of(&accept) + self.padding_of(&accept, 1);
                let reject =
                    self.size_of(&reject) + self.padding_of(&reject, 1);
                1 + accept.max(reject)
            }
            Type::Primitive(p) => p.size(),
            Type::List(_)
            | Type::Table(_)
            | Type::OutputStream(_)
            | Type::Rib(_)
            | Type::BuiltIn(..) => self.pointer_bytes,
            _ => 0,
        }
    }
}