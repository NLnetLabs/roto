use std::{collections::BTreeMap, path::Path};

use crate::{
    ast::{Declaration, Identifier, SyntaxTree},
    parser::{meta::Spans, Parser},
    Parsed, SourceFile,
};

#[derive(Default)]
pub struct ModuleTree {
    modules: Vec<Module>,
}

#[derive(Clone, Copy)]
struct ModuleRef(usize);

struct Module {
    ident: Identifier,
    declarations: BTreeMap<Identifier, Declaration>,
    children: BTreeMap<Identifier, ModuleRef>,
    parent: Option<ModuleRef>,
}

impl Parsed {
    pub fn run(root: &Path) {
        let mut parsed = Self {
            files: Vec::new(),
            modules: ModuleTree::default(),
            spans: Spans::default(),
        };
        let declarations = parsed.read_file(&root.join("lib.roto"));
        parsed.modules.modules.push(Module {
            ident: Identifier::from("lib"),
            declarations,
            children: BTreeMap::new(),
            parent: None,
        });
        parsed.find_submodules(ModuleRef(0), &root);
    }

    fn read_file(
        &mut self,
        path: &Path,
    ) -> BTreeMap<Identifier, Declaration> {
        let file = SourceFile::read(path);
        self.files.push(file);
        let tree = Parser::parse(
            self.files.len() - 1,
            &mut self.spans,
            &self.files.last().unwrap().contents,
        )
        .unwrap();

        Self::map_declarations(tree)
    }

    fn find_submodules(&mut self, parent_id: ModuleRef, path: &Path) {
        for entry in std::fs::read_dir(path).unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();
            if entry.file_type().unwrap().is_dir() {
                self.process_subdir(parent_id, &path);
                continue;
            }

            if path.extension().map_or(false, |ext| ext == "roto") {
                continue;
            }

            let ident = path.file_stem().unwrap().to_str().unwrap();

            if ident == "lib" || ident == "mod" {
                continue;
            }

            let declarations = self.read_file(&path);

            let ident = Identifier::from(ident);
            let module = Module {
                ident,
                declarations,
                children: BTreeMap::new(),
                parent: Some(parent_id),
            };
            let idx = self.modules.modules.len();
            self.modules.modules.push(module);
            self.modules.modules[parent_id.0]
                .children
                .insert(ident, ModuleRef(idx));
        }
    }

    fn process_subdir(&mut self, parent_id: ModuleRef, path: &Path) {
        let ident = path.file_name().unwrap().to_str().unwrap();
        let file_path = path.join("mod.roto");

        if !file_path.exists() {
            return;
        }

        let declarations = self.read_file(&file_path);

        let ident = Identifier::from(ident);
        let module = Module {
            ident,
            declarations,
            children: BTreeMap::new(),
            parent: Some(parent_id),
        };
        let idx = ModuleRef(self.modules.modules.len());
        self.modules.modules.push(module);
        self.modules.modules[parent_id.0]
            .children
            .insert(ident, idx);

        self.find_submodules(idx, path);
    }

    fn map_declarations(
        tree: SyntaxTree,
    ) -> BTreeMap<Identifier, Declaration> {
        let mut map = BTreeMap::new();
        for declaration in tree.declarations {
            let ident = match &declaration {
                Declaration::FilterMap(map) => *map.ident,
                Declaration::Record(ty) => *ty.ident,
                Declaration::Function(func) => *func.ident,
                Declaration::Rib(_)
                | Declaration::Table(_)
                | Declaration::OutputStream(_) => todo!(),
            };
            map.insert(ident, declaration);
        }
        map
    }
}
