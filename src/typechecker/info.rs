use std::{collections::HashMap, fmt::Debug};

use crate::{
    ast::Identifier,
    parser::meta::MetaId,
    runtime::layout::{Layout, LayoutBuilder},
    Runtime,
};

use super::{
    expr::ResolvedPath,
    scope::{DeclarationKind, ResolvedName, ScopeGraph, ScopeRef},
    types::{Function, Primitive, Type, TypeDefinition, TypeName},
    unionfind::UnionFind,
};

/// The output of the type checker that is used for lowering
pub struct TypeInfo {
    /// The unionfind structure that maps type variables to types
    pub(super) unionfind: UnionFind,

    /// All declarations in the program, extracted from the scope graph
    pub scope_graph: ScopeGraph,

    /// Map from type names to types
    pub(super) types: HashMap<ResolvedName, TypeDefinition>,

    /// The types we inferred for each Expr
    ///
    /// This might not be fully resolved yet.
    pub(super) expr_types: HashMap<MetaId, Type>,

    /// The fully qualified (and hence unique) name for each identifier.
    pub(super) resolved_names: HashMap<MetaId, ResolvedName>,

    /// Scopes of functions
    pub(super) function_scopes: HashMap<MetaId, ScopeRef>,

    /// The function that is called on each function call
    pub(super) function_calls: HashMap<MetaId, Function>,

    /// The ids of all the `Expr::Access` nodes that should be interpreted
    /// as enum variant constructors.
    pub(super) path_kinds: HashMap<MetaId, ResolvedPath>,

    pub(super) diverges: HashMap<MetaId, bool>,

    /// Type for return/accept/reject that it constructs and returns.
    pub(super) return_types: HashMap<MetaId, Type>,
}

impl Default for TypeInfo {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeInfo {
    pub fn new() -> Self {
        Self {
            unionfind: UnionFind::default(),
            scope_graph: ScopeGraph::new(),
            types: HashMap::new(),
            expr_types: HashMap::new(),
            resolved_names: HashMap::new(),
            path_kinds: HashMap::new(),
            diverges: HashMap::new(),
            return_types: HashMap::new(),
            function_calls: HashMap::new(),
            function_scopes: HashMap::new(),
        }
    }
}

impl TypeInfo {
    pub fn resolved_name(
        &self,
        x: impl Into<MetaId> + Debug,
    ) -> ResolvedName {
        self.resolved_names[&x.into()]
    }

    pub fn type_of(&mut self, x: impl Into<MetaId> + Debug) -> Type {
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
        &self.function_calls[&x.into()]
    }

    pub fn function_scope(&self, x: impl Into<MetaId>) -> ScopeRef {
        self.function_scopes[&x.into()]
    }

    pub fn path_kind(&self, x: impl Into<MetaId>) -> &ResolvedPath {
        &self.path_kinds[&x.into()]
    }

    pub fn full_name(&self, name: &ResolvedName) -> Identifier {
        let mut s = self.scope_graph.print_scope(name.scope);
        s.push('.');
        s.push_str(name.ident.as_str());
        s.into()
    }

    pub fn is_reference_type(&mut self, ty: &Type, rt: &Runtime) -> bool {
        let ty = self.resolve(ty);
        if self.layout_of(&ty, rt).size() == 0 {
            return false;
        }
        match ty {
            Type::Record(..) | Type::RecordVar(..) => true,
            Type::Name(name) => {
                let type_def = self.resolve_type_name(&name);
                matches!(
                    type_def,
                    TypeDefinition::Enum(..)
                        | TypeDefinition::Record(..)
                        | TypeDefinition::Runtime(..)
                        | TypeDefinition::Primitive(
                            Primitive::IpAddr
                                | Primitive::Prefix
                                | Primitive::String,
                        )
                )
            }
            _ => false,
        }
    }

    pub fn resolve_type_name(&mut self, ty: &TypeName) -> TypeDefinition {
        let name = ty.name;
        let dec = self.scope_graph.get_declaration(name);
        let DeclarationKind::Type(ty) = dec.kind else {
            panic!()
        };
        ty
    }

    pub fn offset_of(
        &mut self,
        record: &Type,
        field: Identifier,
        rt: &Runtime,
    ) -> (Type, u32) {
        let record = self.resolve(record);

        let type_def;
        let fields = match &record {
            Type::Record(fields) | Type::RecordVar(_, fields) => fields,
            Type::Name(type_name) => {
                type_def = self.resolve_type_name(type_name);
                let TypeDefinition::Record(_, fields) = &type_def else {
                    panic!("Can't get offsets in a type that's not a record, but {record}")
                };
                fields
            }
            _ => {
                panic!("Can't get offsets in a type that's not a record, but {record}")
            }
        };

        let mut builder = LayoutBuilder::new();
        for (name, ty) in fields {
            let offset = builder.add(&self.layout_of(ty, rt));
            if name.node == field {
                return (ty.clone(), offset as u32);
            }
        }

        panic!("Field not found")
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
    ///  - Fields are layed out in order, each padded to their alignment.
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
    /// [Rust reference]: https://doc.rust-lang.org/reference/type-layout.html
    pub fn layout_of(&mut self, ty: &Type, rt: &Runtime) -> Layout {
        let ty = self.resolve(ty);
        match ty {
            Type::Var(_) | Type::ExplicitVar(_) => {
                panic!("Can't get the layout of an unconcrete type")
            }
            Type::Function(_, _) => {
                panic!("Can't get the layout of a function type")
            }
            Type::Never => panic!("Can't get the layout of the never type"),
            Type::IntVar(_) => Primitive::i32().layout(),
            Type::RecordVar(_, fields) | Type::Record(fields) => {
                Layout::concat(
                    fields.iter().map(|(_, f)| self.layout_of(f, rt)),
                )
            }
            Type::Name(type_name) => {
                let type_def = self.resolve_type_name(&type_name);
                match type_def {
                    TypeDefinition::Enum(enum_name, variants) => {
                        let subs: Vec<_> = enum_name
                            .arguments
                            .iter()
                            .zip(&type_name.arguments)
                            .collect();

                        let mut layout = Layout::new(0, 1);
                        for variant in &variants {
                            let mut builder = LayoutBuilder::new();
                            builder.add(&Layout::of::<u8>());
                            for field in &variant.fields {
                                builder.add(&self.layout_of(
                                    &field.substitute_many(&subs),
                                    rt,
                                ));
                            }
                            layout = layout.union(&builder.finish());
                        }

                        layout
                    }
                    // TODO: The type constructors need to be instantiated
                    TypeDefinition::Record(_, fields) => Layout::concat(
                        fields.iter().map(|(_, f)| self.layout_of(f, rt)),
                    ),
                    TypeDefinition::Runtime(_, type_id) => {
                        rt.get_runtime_type(type_id).unwrap().layout()
                    }
                    TypeDefinition::Primitive(primitive) => {
                        primitive.layout()
                    }
                }
            }
        }
    }

    pub fn resolve(&mut self, t: &Type) -> Type {
        let mut t = t.clone();

        if let Type::Var(x) | Type::IntVar(x) | Type::RecordVar(x, _) = t {
            t = self.unionfind.find(x).clone();
        }

        t
    }
}
