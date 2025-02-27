use std::collections::BTreeMap;

use crate::{
    ast::{self, Identifier},
    parser::{
        meta::{Meta, Span, Spans},
        Parser,
    },
    FileTree, RotoError, RotoReport,
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

#[derive(Clone, Copy)]
pub struct ModuleRef(pub usize);

pub struct Module {
    pub ident: Meta<Identifier>,
    pub ast: ast::SyntaxTree,
    pub children: BTreeMap<Identifier, ModuleRef>,
    pub parent: Option<ModuleRef>,
}

impl FileTree {
    pub fn parse(self) -> Result<Parsed, RotoReport> {
        Parsed::from_files(self)
    }
}

impl Parsed {
    fn from_files(file_tree: FileTree) -> Result<Self, RotoReport> {
        let mut file_to_mod = BTreeMap::new();
        let mut modules = Vec::new();
        let mut spans = Spans::default();
        let mut errors = Vec::new();

        // First add all modules to the tree
        for (i, file) in file_tree.files.iter().enumerate() {
            let ident: Identifier = (&file.module_name).into();
            let ident = spans.add(
                Span {
                    file: i,
                    start: 0,
                    end: 1,
                },
                ident,
            );

            let ast = match Parser::parse(i, &mut spans, &file.contents) {
                Ok(ast) => ast,
                Err(err) => {
                    errors.push(RotoError::Parse(err));
                    continue;
                }
            };

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
