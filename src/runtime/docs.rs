use std::{fs::File, io, path::Path};

use crate::{
    ice,
    typechecker::{
        scope::{DeclarationKind, ScopeRef, TypeOrStub, ValueKind},
        scoped_display::TypeDisplay,
        types::Type,
    },
};

use super::Runtime;

struct Doc {
    root: DocMod,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum DocItem {
    Const(DocConst),
    Fn(DocFn),
    Ty(DocTy),
    Mod(DocMod),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct DocMod {
    ident: String,
    doc: String,
    items: Vec<DocItem>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct DocTy {
    ident: String,
    doc: String,
    items: Vec<DocItem>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct DocFn {
    ident: String,
    params: Vec<(String, String)>,
    ret: Option<String>,
    doc: String,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct DocConst {
    ident: String,
    ty: String,
    doc: String,
}

impl Doc {
    fn print_md(&self, path: &Path) -> io::Result<()> {
        self.root.print_md(path, true)
    }
}

impl DocFn {
    fn print_md(&self, mut f: impl io::Write) -> io::Result<()> {
        let kind = "function";
        let name = &self.ident;

        let mut parameter_string = String::new();
        let mut first = true;
        for (ident, ty) in &self.params {
            if !first {
                parameter_string.push_str(", ");
            }
            parameter_string.push_str(ident);
            parameter_string.push_str(": ");
            parameter_string.push_str(ty);
            first = false;
        }

        write!(f, "````{{roto:{kind}}} {name}({parameter_string})")?;
        if let Some(ret) = &self.ret {
            writeln!(f, " -> {ret}")?;
        } else {
            writeln!(f)?;
        }
        for line in self.doc.lines() {
            writeln!(f, "{line}")?;
        }
        writeln!(f, "````")?;
        writeln!(f)?;

        Ok(())
    }
}

impl DocMod {
    fn print_md(&self, path: &Path, is_root: bool) -> io::Result<()> {
        use std::io::Write;

        let path = if is_root {
            path.to_path_buf()
        } else {
            path.join(&self.ident)
        };

        std::fs::create_dir(&path).unwrap();
        let file_path = path.join("index").with_extension("md");
        let mut file = File::create(file_path)?;

        writeln!(file, "# {}", self.ident)?;
        writeln!(file)?;
        writeln!(file, "{}", self.doc)?;

        let (fs, cs, ts, ms) = Runtime::split_items(&self.items);

        if !ms.is_empty() {
            writeln!(file, "## Modules ")?;
            writeln!(file, "```{{toctree}}")?;
            writeln!(file, ":maxdepth: 1")?;
            for m in &ms {
                writeln!(
                    file,
                    "{} <{}>",
                    m.ident,
                    Path::new(&m.ident).join("index").display()
                )?;
            }
            writeln!(file, "```")?;
        }

        if !ts.is_empty() {
            writeln!(file, "## Types")?;
            writeln!(file, "```{{toctree}}")?;
            writeln!(file, ":maxdepth: 1")?;
            for t in &ts {
                writeln!(
                    file,
                    "{} <{}>",
                    t.ident,
                    Path::new(&t.ident).join("index").display()
                )?;
            }
            writeln!(file, "```")?;
        }

        if !cs.is_empty() {
            writeln!(file, "## Constants")?;
            for c in &cs {
                c.print_md(&mut file)?;
            }
        }

        if !fs.is_empty() {
            writeln!(file, "## Functions")?;
            for f in &fs {
                f.print_md(&mut file)?;
            }
        }

        drop(file);

        for m in &ms {
            m.print_md(&path, false)?;
        }
        for t in &ts {
            t.print_md(&path)?;
        }

        Ok(())
    }
}

impl DocConst {
    fn print_md(&self, mut f: impl std::io::Write) -> io::Result<()> {
        writeln!(f, "`````{{roto:constant}} {}: {}", self.ident, self.ty,)?;
        for line in self.doc.lines() {
            writeln!(f, "{line}")?;
        }
        writeln!(f, "`````\n")?;

        Ok(())
    }
}

impl DocTy {
    fn print_md(&self, path: &Path) -> io::Result<()> {
        use std::io::Write;
        let path = path.join(&self.ident);

        std::fs::create_dir(&path)?;
        let file_path = path.join("index").with_extension("md");
        let mut file = File::create(file_path)?;

        writeln!(file, "# {}", self.ident)?;
        writeln!(file, "`````{{roto:type}} {}", self.ident)?;
        for line in self.doc.lines() {
            writeln!(file, "{line}")?;
        }
        writeln!(file, "`````\n")?;
        writeln!(file)?;

        let (fs, _, _, _) = Runtime::split_items(&self.items);
        for f in &fs {
            f.print_md(&mut file)?;
        }

        Ok(())
    }
}

impl Runtime {
    fn print_ty(&self, ty: &impl TypeDisplay) -> String {
        ty.display(&self.type_checker.type_info).to_string()
    }

    fn build_doc(&self) -> Doc {
        let items = self.get_items(ScopeRef::GLOBAL);

        Doc {
            root: DocMod {
                ident: "Standard Library".into(),
                doc: "".into(),
                items,
            },
        }
    }

    fn get_items(&self, scope: ScopeRef) -> Vec<DocItem> {
        let mut out = Vec::new();

        let graph = self.type_checker.get_scope_graph();
        let decs = graph.declarations_in(scope);

        for dec in decs {
            match &dec.kind {
                DeclarationKind::Value(value_kind, ty) => match value_kind {
                    ValueKind::Local => unreachable!(), // won't happen
                    ValueKind::Constant => {
                        out.push(DocItem::Const(DocConst {
                            ident: dec.name.ident.as_str().into(),
                            ty: self.print_ty(ty),
                            doc: dec.doc.clone(),
                        }));
                    }
                    ValueKind::Context(_) => {
                        out.push(DocItem::Const(DocConst {
                            ident: dec.name.ident.as_str().into(),
                            ty: self.print_ty(ty),
                            doc: dec.doc.clone(),
                        }));
                    }
                },
                DeclarationKind::Type(t) => {
                    let TypeOrStub::Type(ty) = t else { ice!() };
                    let scope = self
                        .type_checker
                        .get_scope_of(dec.name.scope, dec.name.ident)
                        .unwrap();
                    let items = self.get_items(scope);

                    out.push(DocItem::Ty(DocTy {
                        ident: self.print_ty(ty),
                        doc: dec.doc.clone(),
                        items,
                    }));
                }
                DeclarationKind::Method(f) | DeclarationKind::Function(f) => {
                    let f = f.as_ref().unwrap();
                    let Type::Function(params, ret) = &f.ty else {
                        ice!()
                    };

                    let params = f
                        .parameter_names
                        .iter()
                        .zip(params)
                        .map(|(n, ty)| (n.as_str().into(), self.print_ty(ty)))
                        .collect();
                    let ret = self.print_ty(&**ret);
                    let ret = if ret == "()" { None } else { Some(ret) };

                    out.push(DocItem::Fn(DocFn {
                        ident: dec.name.ident.as_str().into(),
                        params,
                        ret,
                        doc: dec.doc.clone(),
                    }));
                }
                DeclarationKind::Module => {
                    let scope = self
                        .type_checker
                        .get_scope_of(dec.name.scope, dec.name.ident)
                        .unwrap();
                    let items = self.get_items(scope);

                    out.push(DocItem::Mod(DocMod {
                        ident: dec.name.ident.as_str().into(),
                        doc: dec.doc.clone(),
                        items,
                    }));
                }
                DeclarationKind::Variant(_, _) => {
                    // Skip for now
                }
            }
        }

        out
    }

    fn split_items(
        items: &[DocItem],
    ) -> (Vec<&DocFn>, Vec<&DocConst>, Vec<&DocTy>, Vec<&DocMod>) {
        let mut fs = Vec::new();
        let mut cs = Vec::new();
        let mut ts = Vec::new();
        let mut ms = Vec::new();

        for item in items {
            match item {
                DocItem::Const(c) => cs.push(c),
                DocItem::Fn(f) => fs.push(f),
                DocItem::Ty(t) => ts.push(t),
                DocItem::Mod(m) => ms.push(m),
            }
        }

        fs.sort();
        cs.sort();
        ts.sort();
        ms.sort();

        (fs, cs, ts, ms)
    }

    /// Print documentation for all the types registered into this runtime.
    ///
    /// The format for the documentation is markdown that can be passed to Sphinx.
    pub fn print_documentation(&self, path: &Path) -> io::Result<()> {
        if path.exists() {
            eprintln!("Delete the directory first!");
            return Ok(());
        }
        let doc = self.build_doc();
        doc.print_md(path)
    }
}
