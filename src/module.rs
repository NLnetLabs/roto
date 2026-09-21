//! Module tree of a Roto script

use std::collections::BTreeMap;

use crate::{
    FileTree, RotoError, RotoReport,
    ast::{self, Declaration, Identifier, SyntaxTree, YangModuleDeclaration},
    parser::{
        ParseError,
        meta::{Meta, Span, Spans},
    },
    yang::parser::YangParser,
};

pub struct Parsed {
    pub module_tree: ModuleTree,
    pub file_tree: FileTree,
    pub spans: Spans,
}

#[derive(Default)]
pub struct ModuleTree {
    pub modules: Vec<Module>,
}

#[derive(Debug, Clone, Copy)]
pub struct ModuleRef(pub usize);

#[derive(Debug)]
pub struct Module {
    pub ident: Meta<Identifier>,
    pub prefix: Meta<Identifier>,
    pub namespace: Meta<String>,
    pub ast: ast::SyntaxTree,
    pub children: BTreeMap<Identifier, ModuleRef>,
    pub parent: Option<ModuleRef>,
}

impl FileTree {
    /// Parse the files in the [`FileTree`] returning the AST.
    pub fn parse(self) -> Result<Parsed, RotoReport> {
        Parsed::from_files(self)
    }

    /// Parse the files in the [`FileTree`] with modules defined in them, as
    /// yang presuppes, returning the AST.
    pub fn parse_with_modules(self) -> Result<Parsed, RotoReport> {
        Parsed::from_declared_modules(self)
    }
}

impl Parsed {
    fn from_files(file_tree: FileTree) -> Result<Self, RotoReport> {
        let mut file_to_mod = BTreeMap::new();
        let mut modules = Vec::new();
        let mut spans = Spans::default();
        let mut errors: Vec<RotoError> = Vec::new();

        // First add all modules to the tree
        for (i, file) in file_tree.files.iter().enumerate() {
            let ident: Identifier = (&file.module_name).into();
            // let mut ident = spans.add(
            //     Span {
            //         file: i,
            //         start: 0,
            //         end: 1,
            //     },
            //     ident,
            // );

            let ast = match YangParser::parse(i, &mut spans, &file.contents) {
                Ok(ast) => ast,
                Err(err) => {
                    errors.push(RotoError::Parse(*err));
                    continue;
                }
            };

            if let Declaration::YangModule(YangModuleDeclaration {
                ident,
                prefix,
                namespace,
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

                file_to_mod.insert(i, modules.len());

                modules.push(Module {
                    ident: ident.clone(),
                    prefix: prefix.clone().unwrap(),
                    namespace: namespace.clone().unwrap(),
                    children: BTreeMap::new(),
                    parent: None,
                    ast,
                })
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

    fn from_declared_modules(
        file_tree: FileTree,
    ) -> Result<Parsed, RotoReport> {
        let mut modules = Vec::new();
        let mut spans = Spans::default();
        let mut errors: Vec<RotoError> = Vec::new();
        let mut mod_iter = 0;

        // we're doing two runs over the files, one to get all modules, and
        // the second run to get all submodules, so that we can immediately
        // check in the second run if the `belongs-to` attributes on the
        // submodule actually exists.
        while mod_iter <= 1 {
            for (i, file) in file_tree.files.iter().enumerate() {
                let ident: Identifier = (&file.module_name).into();
                let start = spans.add(
                    Span {
                        file: i,
                        start: 0,
                        end: 1,
                    },
                    ident,
                );
                let ast =
                    match YangParser::parse(i, &mut spans, &file.contents) {
                        Ok(ast) => ast,
                        Err(err) => {
                            errors.push(RotoError::Parse(*err));
                            continue;
                        }
                    };

                for (
                    mi,
                    YangModuleDeclaration {
                        ident,
                        prefix,
                        namespace,
                        parent,
                        ..
                    },
                ) in ast.yang_modules().enumerate()
                {
                    let module_ast = SyntaxTree {
                        declarations: [ast.declarations[mi].clone()].to_vec(),
                    };

                    match parent {
                        // no parent means it has to be a module
                        None if mod_iter == 0 => {
                            modules.push(Module {
                                prefix: prefix.clone().unwrap(),
                                namespace: namespace.clone().unwrap(),
                                parent: None,
                                ident: ident.clone(),
                                ast: module_ast,
                                children: BTreeMap::new(),
                            });
                        }
                        // found a parent: this must be a submodule
                        Some(p) if mod_iter == 1 => {
                            let Some((parent_id, pm_dec)) = modules
                                .iter()
                                .enumerate()
                                .find(|(_, ym)| ym.ident.node == p.node)
                            else {
                                let span = spans.merge(start.id, p);
                                errors.push(RotoError::Parse(
                                    ParseError::expected(
                                        "a defined module",
                                        p.node,
                                        span,
                                    ),
                                ));
                                continue;
                            };

                            modules.push(Module {
                                prefix: pm_dec.prefix.clone(),
                                namespace: pm_dec.namespace.clone(),
                                parent: Some(ModuleRef(parent_id)),
                                ident: ident.clone(),
                                ast: module_ast,
                                children: BTreeMap::new(),
                            });
                        }
                        _ => {}
                    };
                }
            }

            if !errors.is_empty() {
                return Err(RotoReport {
                    files: file_tree.files,
                    errors,
                    spans,
                });
            }

            mod_iter += 1;
        }

        println!(
            "module {:#?}",
            modules
                .iter()
                .enumerate()
                .map(|(i, m)| format!(
                    "{i} {} ({}) <- {:?}",
                    m.ident, m.prefix, m.parent
                ))
                .collect::<Vec<_>>()
        );

        Ok(Self {
            module_tree: ModuleTree { modules },
            file_tree,
            spans,
        })
    }
}
