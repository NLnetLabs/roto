//! Module tree of a Roto script

use std::collections::BTreeMap;

use ariadne::IndexType;

use crate::ast::{Declaration, Stmt, SyntaxTree, YangModuleDeclaration};
use crate::yang::parser::ast::Argument;
use crate::yang::yang_file_tree::YangFileTree;
use crate::{FileTree, RotoError};
use crate::{
    RotoReport,
    ast::Identifier,
    parser::meta::{Meta, Span, Spans},
    yang::YangParser,
};

pub struct Parsed {
    pub module_tree: ModuleTree,
    pub file_tree: YangFileTree,
    pub spans: Spans,
}

#[derive(Default)]
pub struct ModuleTree {
    pub modules: Vec<Module>,
}

#[derive(Clone, Copy)]
pub struct ModuleRef(pub usize);

pub struct Module {
    pub ident: Meta<Identifier>,
    pub ast: SyntaxTree,
    pub children: BTreeMap<Identifier, ModuleRef>,
    pub parent: Option<ModuleRef>,
}

// impl FileTree {
/// Parse the files in the [`FileTree`] returning the AST.
// pub fn parse(self) -> Result<Parsed, RotoReport> {
//     Parsed::from_files(self)
// }

/// Parse the files in the [`FileTree`] with modules defined in them.
/// Returns the AST.
//     pub fn parse_with_modules(self) -> Result<Parsed, RotoReport> {
//         Parsed::from_declared_modules(self)
//     }
// }

impl Parsed {
    fn from_declared_modules(
        file_tree: YangFileTree,
    ) -> Result<Parsed, RotoReport> {
        let mut modules = Vec::new();
        let mut spans = Spans::default();
        let mut errors: Vec<RotoError> = Vec::new();

        for (i, file) in file_tree.files.iter().enumerate() {
            let ast = match YangParser::parse(i, &mut spans, &file.contents) {
                Ok(ast) => ast,
                Err(err) => {
                    errors.push(RotoError::Parse(*err));
                    continue;
                }
            };

            for (mi, YangModuleDeclaration { ident, body }) in
                ast.yang_modules().enumerate()
            {
                // let ident = module.node;
                // let ident = spans.add(
                //     Span {
                //         file: i,
                //         start: module.0.id.0,
                //         end: module.0.id.0 + 1,
                //     },
                //     ident,
                // );

                let module_ast = SyntaxTree {
                    declarations: [ast.declarations[mi].clone()].to_vec(),
                };

                modules.push(Module {
                    ident: ident.clone(),
                    children: BTreeMap::new(),
                    parent: None,
                    ast: module_ast,
                });
            }
        }

        if !errors.is_empty() {
            return Err(RotoReport {
                files: file_tree.files,
                errors,
                spans,
            });
        }

        // Then wire up all the relations between the modules
        for (parent, file) in file_tree.files.iter().enumerate() {
            for child in &file.children {
                let child_module = &mut modules[*child];
                child_module.parent = Some(ModuleRef(parent));
                let child_ident = *child_module.ident;
                modules[parent]
                    .children
                    .insert(child_ident, ModuleRef(*child));
            }
        }

        Ok(Self {
            module_tree: ModuleTree { modules },
            file_tree,
            spans,
        })
    }

    fn from_files(file_tree: YangFileTree) -> Result<Self, RotoReport> {
        let mut file_to_mod = BTreeMap::new();
        let mut modules = Vec::new();
        let mut spans = Spans::default();
        let mut errors: Vec<RotoError> = Vec::new();

        // First add all modules to the tree
        for (i, file) in file_tree.files.iter().enumerate() {
            let ident: Identifier = (&file.module_name).into();
            let mut ident = spans.add(
                Span {
                    file: i,
                    start: 0,
                    end: 1,
                },
                ident,
            );

            let ast = match YangParser::parse(i, &mut spans, &file.contents) {
                Ok(ast) => ast,
                Err(err) => {
                    errors.push(RotoError::Parse(*err));
                    continue;
                }
            };

            if let Declaration::YangModule(YangModuleDeclaration {
                ident,
                ..
            }) = &ast.declarations[0]
            {
                spans.add(
                    Span {
                        file: i,
                        start: 0,
                        end: 1,
                    },
                    ident,
                );
            }

            file_to_mod.insert(i, modules.len());

            modules.push(Module {
                ident,
                children: BTreeMap::new(),
                parent: None,
                ast,
            })
        }

        if !errors.is_empty() {
            return Err(RotoReport {
                files: file_tree.files,
                errors,
                spans,
            });
        }

        // Then wire up all the relations between the modules
        for (parent, file) in file_tree.files.iter().enumerate() {
            for child in &file.children {
                let child_module = &mut modules[*child];
                child_module.parent = Some(ModuleRef(parent));
                let child_ident = *child_module.ident;
                modules[parent]
                    .children
                    .insert(child_ident, ModuleRef(*child));
            }
        }

        Ok(Self {
            module_tree: ModuleTree { modules },
            file_tree,
            spans,
        })
    }
}
