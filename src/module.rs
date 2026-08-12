//! Module tree of a Roto script

use std::collections::{BTreeMap, HashMap};

use crate::{
    FileTree, RotoError, RotoReport, SourceFile,
    ast::{self, Identifier},
    deps::DepGraph,
    parser::{
        Parser,
        meta::{Meta, Span, Spans},
    },
};

#[derive(Debug)]
pub struct Parsed {
    pub module_tree: ModuleTree,
    pub files: Vec<SourceFile>,
    pub spans: Spans,
}

#[derive(Debug)]
pub struct ModuleTree {
    pub main: Identifier,
    pub deps: HashMap<Identifier, Vec<Identifier>>,
    pub pkgs: HashMap<Identifier, ModuleRef>,
    pub modules: Vec<Module>,
}

#[derive(Debug, Clone, Copy)]
pub struct ModuleRef(pub usize);

#[derive(Debug)]
pub struct Module {
    pub ident: Meta<Identifier>,
    pub ast: ast::SyntaxTree,
    pub children: BTreeMap<Identifier, ModuleRef>,
    pub parent: Option<ModuleRef>,
    pub package: Identifier,
}

impl Parsed {
    pub(crate) fn from_files(
        mut graph: DepGraph,
    ) -> Result<Self, RotoReport> {
        let mut errors = Vec::new();

        let mut this = Self {
            module_tree: ModuleTree {
                main: graph.main,
                deps: graph.deps,
                pkgs: HashMap::new(),
                modules: Vec::new(),
            },
            files: Vec::new(),
            spans: Spans::default(),
        };

        let main = graph.pkgs.remove(&graph.main).unwrap();
        let offset = this.process_filetree(graph.main, &mut errors, main);
        this.module_tree.pkgs.insert(graph.main, ModuleRef(offset));
        assert_eq!(offset, 0);

        for (name, dep) in graph.pkgs {
            let offset = this.process_filetree(name, &mut errors, dep);
            this.module_tree.pkgs.insert(name, ModuleRef(offset));
        }

        if !errors.is_empty() {
            return Err(RotoReport {
                files: this.files,
                spans: this.spans,
                errors,
            });
        }

        Ok(this)
    }

    fn process_filetree(
        &mut self,
        package: Identifier,
        errors: &mut Vec<RotoError>,
        tree: FileTree,
    ) -> usize {
        let offset = self.module_tree.modules.len();

        // First add all modules to the tree
        for (i, file) in tree.files.iter().enumerate() {
            let i = i + offset;

            self.files.push(file.clone());

            let ident: Identifier = (&file.module_name).into();
            let ident = self.spans.add(
                Span {
                    file: i,
                    start: 0,
                    end: 1,
                },
                ident,
            );

            let res = Parser::parse(i, &mut self.spans, &file.contents);

            let ast = match res {
                Ok(ast) => ast,
                Err(err) => {
                    errors.push(RotoError::Parse(*err));
                    continue;
                }
            };

            self.module_tree.modules.push(Module {
                ident,
                children: BTreeMap::new(),
                parent: None,
                ast,
                package,
            })
        }

        if !errors.is_empty() {
            return offset;
        }

        // Then wire up all the relations between the modules
        for (parent, file) in tree.files.iter().enumerate() {
            let parent = parent + offset;
            for child in &file.children {
                let child = child + offset;
                let child_module = &mut self.module_tree.modules[child];
                child_module.parent = Some(ModuleRef(parent));
                let child_ident = *child_module.ident;
                self.module_tree.modules[parent]
                    .children
                    .insert(child_ident, ModuleRef(child));
            }
        }

        offset
    }
}
