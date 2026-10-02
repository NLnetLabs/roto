//! Module tree of a Roto script

use std::{
    collections::{BTreeMap, HashMap},
    path::{Path, PathBuf},
};

use crate::{
    FileSpec, FileTree, RotoError, RotoReport, SourceFile, YangFiles,
    ast::{
        self, Declaration, Identifier, SyntaxTree, YangModuleDeclaration,
        YangSubModuleDeclaration,
    },
    file_tree::{YangModuleSpec, custom_error, read_error},
    parser::{
        ParseError,
        meta::{Meta, MetaId, Span, Spans},
    },
    typechecker::error::Label,
    yang::parser::YangParser,
};

pub struct Parsed {
    pub module_tree: ModuleTree,
    pub file_tree: YangFiles,
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

    // pub fn parse_with_modules(self) -> Result<Parsed, RotoReport> {
    //     Parsed::from_module_files(self)
    // }
}

impl Parsed {
    /// docs here
    pub fn from_entry_point(
        entry_point_file: &Path,
        lib_path: &Path,
    ) -> Result<Self, RotoReport> {
        let mut errors: Vec<RotoError> = Vec::new();
        let mut modules = Vec::new();
        let mut spans = Spans::default();
        let mut files = HashMap::new();

        let entry_point_str =
            entry_point_file.file_name().unwrap().to_str().unwrap();
        // create a collection of file names that potentially havee modules
        // in them
        let mut lib =
            YangFiles::create_yang_lib(lib_path, entry_point_str, "yang")?;

        // load & parse the entry point file
        let (entry_point, entry_point_idx) = lib
            .try_get_or_load(&YangFiles::as_ident(entry_point_str).unwrap())
            .map_err(|e| read_error(entry_point_file.to_path_buf(), e))?;

        files.insert(entry_point_idx, entry_point.clone());
        println!(
            "[from_entry_point] inserted entry point module with idx {}",
            entry_point_idx
        );

        let (ast, mut imported_modules) = match YangParser::parse(
            entry_point_idx,
            &mut spans,
            &entry_point.contents,
        ) {
            Ok((ast, i_mods)) => (ast, vec![(entry_point_idx, i_mods)]),
            Err(err) => {
                errors.push(RotoError::Parse(*err));
                return Err(RotoReport {
                    files,
                    errors,
                    spans,
                });
            }
        };

        // add entry point module to the parsed modules
        spans =
            Parsed::add_modules(&ast, spans, entry_point_idx, &mut modules)?;

        // now, go over the imported modules in the entry point module, and
        // descendd into those if we find more imported modules in them
        // loop {
        println!(
            "[from_entry_point] found imported modules {:?}",
            imported_modules
        );
        // go over all module imports found in the previous imported
        // module
        loop {
            let mut new_imported_modules = vec![];
            for (parent_file, i_mods) in imported_modules.clone() {
                for module_path in i_mods {
                    // look the module up in the library
                    let (search_mod, file_idx) = lib
                        .try_get_or_load(&module_path.node)
                        .map_err(|e| RotoReport {
                            errors: vec![RotoError::Parse(
                                ParseError::custom(
                                    format!(
                                        "{} in module `{}`",
                                        e,
                                        files
                                            .get(&parent_file)
                                            .map(|sf| &sf.name)
                                            .unwrap_or(
                                                &"<NO MODULE NAME>"
                                                    .to_string()
                                            )
                                    ),
                                    "this import",
                                    spans.get(module_path.id),
                                ),
                            )],
                            files: files.clone(),
                            ..Default::default()
                        })?;

                    match YangParser::parse(
                        file_idx,
                        &mut spans,
                        &search_mod.contents,
                    ) {
                        Ok((ast, mods)) => {
                            if let std::collections::hash_map::Entry::Vacant(
                                entry,
                            ) = files.entry(file_idx)
                            {
                                new_imported_modules.push((file_idx, mods));
                                entry.insert(search_mod.clone());

                                spans = Self::add_modules(
                                    &ast,
                                    spans,
                                    file_idx,
                                    &mut modules,
                                )?;
                                // files.insert(file_idx, search_mod.clone());
                                println!(
                                    "[from_entry_point] inserted imported \
                                    module with idx {file_idx} name {} for \
                                    parent {:?}",
                                    search_mod.name,
                                    files
                                        .get(&parent_file)
                                        .map(|sf| &sf.name)
                                );
                            }
                        }
                        Err(err) => {
                            if let std::collections::hash_map::Entry::Vacant(
                                entry,
                            ) = files.entry(file_idx)
                            {
                                entry.insert(search_mod.clone());
                            }
                            errors.push(RotoError::Parse(*err));
                            return Err(RotoReport {
                                files,
                                errors,
                                spans,
                            });
                        }
                    };
                }
            }
            if new_imported_modules.is_empty() {
                break;
            }
            imported_modules.extend(new_imported_modules.clone());
        }

        println!("[from_entry_point] done parsing");
        println!(
            "lib {:#?}",
            lib.lib
                .iter()
                .enumerate()
                .map(|(i, m)| (i, m.0))
                .collect::<Vec<_>>()
        );
        println!(
            "modules {:?}",
            modules.iter().map(|m| &m.ident).collect::<Vec<_>>()
        );
        modules.reverse();
        Ok(Self {
            module_tree: ModuleTree { modules },
            file_tree: lib,
            spans,
        })
    }

    fn add_modules(
        ast: &SyntaxTree,
        mut spans: Spans,
        file: usize,
        modules: &mut Vec<Module>,
    ) -> Result<Spans, RotoReport> {
        let mut errors: Vec<RotoError> = Vec::new();
        for module_decl in &ast.declarations {
            if let Declaration::YangModule(YangModuleDeclaration {
                ident,
                prefix,
                namespace,
                ..
            }) = module_decl
            {
                spans.add(
                    Span {
                        file,
                        start: 0,
                        end: 1,
                    },
                    ident.clone(),
                );

                modules.push(Module {
                    ident: ident.clone(),
                    prefix: prefix.clone(),
                    namespace: namespace.clone(),
                    children: BTreeMap::new(),
                    parent: None,
                    ast: SyntaxTree {
                        declarations: vec![module_decl.clone()],
                    },
                });
            }
        }

        for (child, sub_module_decl) in ast.declarations.iter().enumerate() {
            if let Declaration::YangSubModule(YangSubModuleDeclaration {
                ident,
                belongs_to,
                ..
            }) = sub_module_decl
            {
                spans.add(
                    Span {
                        file,
                        start: 0,
                        end: 1,
                    },
                    ident.clone(),
                );

                let mut parent_id = None;
                for (i, m) in modules.iter_mut().enumerate() {
                    if &m.ident == belongs_to {
                        m.children.insert(ident.node, ModuleRef(child));
                        parent_id = Some(i);
                    }
                }

                match parent_id {
                    None => {
                        errors.push(RotoError::Parse(ParseError::custom(
                            format!(
                                "Parent module `{belongs_to}` cannot be found"
                            ),
                            "in this module",
                            Span {
                                file,
                                start: 0,
                                end: 1,
                            },
                        )));
                        continue;
                    }
                    Some(parent_id) => {
                        modules.push(Module {
                            ident: ident.clone(),
                            prefix: modules[parent_id].prefix.clone(),
                            namespace: modules[parent_id].namespace.clone(),
                            children: BTreeMap::new(),
                            parent: Some(ModuleRef(parent_id)),
                            ast: SyntaxTree {
                                declarations: vec![sub_module_decl.clone()],
                            },
                        });
                    }
                };
            }
        }

        Ok(spans)
    }

    fn from_files(file_tree: FileTree) -> Result<Self, RotoReport> {
        todo!()
        // let mut file_to_mod = BTreeMap::new();
        // let mut modules = Vec::new();
        // let mut spans = Spans::default();
        // let mut errors: Vec<RotoError> = Vec::new();

        // // First add all modules to the tree
        // for (i, file) in file_tree.files.iter().enumerate() {
        //     let ident: Identifier = (&file.module_name).into();
        //     // let mut ident = spans.add(
        //     //     Span {
        //     //         file: i,
        //     //         start: 0,
        //     //         end: 1,
        //     //     },
        //     //     ident,
        //     // );

        //     let ast = match YangParser::parse(i, &mut spans, &file.contents) {
        //         Ok(ast) => ast,
        //         Err(err) => {
        //             errors.push(RotoError::Parse(*err));
        //             continue;
        //         }
        //     };

        //     if let Declaration::YangModule(YangModuleDeclaration {
        //         ident,
        //         prefix,
        //         namespace,
        //         ..
        //     }) = &ast.declarations[0]
        //     {
        //         spans.add(
        //             Span {
        //                 file: i,
        //                 start: 0,
        //                 end: 1,
        //             },
        //             ident,
        //         );

        //         file_to_mod.insert(i, modules.len());

        //         modules.push(Module {
        //             ident: ident.clone(),
        //             prefix: prefix.clone(),
        //             namespace: namespace.clone(),
        //             children: BTreeMap::new(),
        //             parent: None,
        //             ast,
        //         })
        //     }
        // }

        // if !errors.is_empty() {
        //     return Err(RotoReport {
        //         files: file_tree.files,
        //         errors,
        //         spans,
        //     });
        // }

        // // Then wire up all the relations between the modules
        // for (parent, file) in file_tree.files.iter().enumerate() {
        //     for child in &file.children {
        //         let child_module = &mut modules[*child];
        //         child_module.parent = Some(ModuleRef(parent));
        //         let child_ident = *child_module.ident;
        //         modules[parent]
        //             .children
        //             .insert(child_ident, ModuleRef(*child));
        //     }
        // }

        // Ok(Self {
        //     module_tree: ModuleTree { modules },
        //     file_tree,
        //     spans,
        // })
    }

    fn from_module_files(file_tree: YangFiles) -> Result<Parsed, RotoReport> {
        let mut modules = Vec::new();
        let mut spans = Spans::default();
        let mut errors: Vec<RotoError> = Vec::new();
        let mut mod_iter = 0;

        // we're doing two runs over the files, one to get all modules, and
        // the second run to get all submodules, so that we can immediately
        // check on the second run if the `belongs-to` attributes on the
        // submodule actually exists.
        while mod_iter <= 1 {
            for (i, file) in file_tree.iter_parsed().enumerate() {
                let ident: Identifier = (&file.module_name).into();
                let start = spans.add(
                    Span {
                        file: i,
                        start: 0,
                        end: 1,
                    },
                    ident,
                );
                let (ast, imported_modules) =
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
                                prefix: prefix.clone(),
                                namespace: namespace.clone(),
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
                    files: file_tree.into(),
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
