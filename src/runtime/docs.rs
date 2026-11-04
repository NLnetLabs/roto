use std::{cmp::Ordering, fs::File, io, path::Path};

use crate::{
    ice,
    runtime::{OptCtx, Rt},
    typechecker::{
        scope::{DeclarationKind, ScopeRef, TypeOrStub, ValueKind},
        scoped_display::TypeDisplay,
        types::{self, Type},
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
    args: Vec<String>,
    doc: String,
    items: Vec<DocItem>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct DocFn {
    receiver: Option<String>,
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
        let kind = if self.receiver.is_some() {
            "method"
        } else {
            "function"
        };

        let name = if let Some(receiver) = &self.receiver {
            &format!("{receiver}.{}", self.ident)
        } else {
            &self.ident
        };

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

        let (fs, cs, ts, ms) = Rt::split_items(&self.items);

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
            writeln!(file, ":hidden: 1")?;
            for t in &ts {
                writeln!(
                    file,
                    "{} <{}>",
                    t.ident,
                    Path::new(&t.ident).join("index").display()
                )?;
            }
            writeln!(file, "```")?;

            writeln!(file)?;
            writeln!(file, ":::{{list-table}}")?;
            writeln!(file, ":class: roto-contents")?;
            for t in &ts {
                writeln!(
                    file,
                    "* - [`{}`]({})",
                    t.ident,
                    Path::new(&t.ident).join("index").display(),
                )?;
                writeln!(file, "  - {}", doc_slug(&t.doc),)?;
            }
            writeln!(file, ":::")?;
            writeln!(file)?;
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

fn doc_slug(s: &str) -> String {
    s.split_once("\n\n").map_or(s, |s| s.0).into()
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

        let (fs, cs, _, _) = Rt::split_items(&self.items);

        for c in &cs {
            c.print_md(&mut file)?;
        }
        for f in &fs {
            f.print_md(&mut file)?;
        }

        Ok(())
    }
}

impl<Ctx: OptCtx> Runtime<Ctx> {
    /// Print documentation for all the types registered into this runtime.
    ///
    /// The format for the documentation is markdown that can be passed to Sphinx.
    pub fn print_documentation(&self, path: &Path) -> io::Result<()> {
        self.rt.print_documentation(path)
    }
}

impl Rt {
    pub fn print_documentation(&self, path: &Path) -> io::Result<()> {
        if path.exists() {
            eprintln!("Delete the directory first!");
            return Ok(());
        }
        let doc = self.build_doc();
        doc.print_md(path)
    }

    fn print_ty(&self, ty: &impl TypeDisplay) -> String {
        ty.display(ScopeRef::GLOBAL, &self.type_checker.type_info)
            .to_string()
    }

    fn build_doc(&self) -> Doc {
        let items = self.get_items(ScopeRef::STD, None);

        Doc {
            root: DocMod {
                ident: "Standard Library".into(),
                doc: "".into(),
                items,
            },
        }
    }

    fn get_items(&self, scope: ScopeRef, ty: Option<String>) -> Vec<DocItem> {
        let mut out = Vec::new();

        let graph = self.type_checker.get_scope_graph();
        let decs = graph.declarations_in(scope);

        for dec in decs {
            match &dec.kind {
                DeclarationKind::Value(_, None) => {
                    ice!(
                        "There shouldn't be any stub declarations left \
                        otherwise there would be an issue with the type \
                        checker."
                    )
                }
                DeclarationKind::Value(value_kind, Some(ty)) => {
                    match value_kind {
                        ValueKind::Local => ice!(
                            "We can't generate documentation for local\
                            variables since a Runtime shouldn't have any."
                        ),
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
                    }
                }
                DeclarationKind::Type(t) => {
                    let TypeOrStub::Type(ty) = t else { ice!() };
                    let scope = self
                        .type_checker
                        .get_scope_of(dec.name.scope, dec.name.ident)
                        .unwrap();
                    let items = self.get_items(
                        scope,
                        Some(ty.type_name().name.ident.to_string()),
                    );

                    out.push(DocItem::Ty(DocTy {
                        ident: ty.type_name().name.ident.to_string(),
                        args: ty
                            .type_name()
                            .arguments
                            .iter()
                            .map(|t| {
                                let Type::ExplicitVar(s) = t else {
                                    panic!();
                                };
                                s.to_string()
                            })
                            .collect(),
                        doc: dec.doc.clone(),
                        items,
                    }));
                }
                DeclarationKind::Method(f) | DeclarationKind::Function(f) => {
                    let f = f.as_ref().unwrap();
                    let types::Signature {
                        types: _,
                        parameter_types,
                        return_type,
                    } = &f.signature;

                    let params = f
                        .parameter_names
                        .iter()
                        .zip(parameter_types)
                        .map(|(n, ty)| (n.as_str().into(), self.print_ty(ty)))
                        .collect();
                    let ret = self.print_ty(return_type);
                    let ret = if ret == "()" { None } else { Some(ret) };

                    out.push(DocItem::Fn(DocFn {
                        receiver: ty.clone(),
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
                    let items = self.get_items(scope, None);

                    out.push(DocItem::Mod(DocMod {
                        ident: dec.name.ident.as_str().into(),
                        doc: dec.doc.clone(),
                        items,
                    }));
                }
                DeclarationKind::Enum(_) => {
                    // Skip for now
                }
                DeclarationKind::TypeParam(_) => {
                    // Probably skipped forever
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

        fs.sort_by(|a, b| version_cmp(&a.ident, &b.ident));
        cs.sort_by(|a, b| version_cmp(&a.ident, &b.ident));
        ts.sort_by(|a, b| version_cmp(&a.ident, &b.ident));
        ms.sort_by(|a, b| version_cmp(&a.ident, &b.ident));

        (fs, cs, ts, ms)
    }
}

fn version_cmp(a: &str, b: &str) -> Ordering {
    // 1. Compare leading non-numerical part
    // 2. Compare leading numerical part
    // 3. Repeat
    let mut a = a.as_bytes();
    let mut b = b.as_bytes();
    while !a.is_empty() || !b.is_empty() {
        let a_numerical_start =
            a.iter().position(u8::is_ascii_digit).unwrap_or(a.len());
        let b_numerical_start =
            b.iter().position(u8::is_ascii_digit).unwrap_or(b.len());

        let a_str = &a[..a_numerical_start];
        let b_str = &b[..b_numerical_start];

        match a_str.cmp(b_str) {
            Ordering::Equal => {}
            ord => return ord,
        }

        a = &a[a_numerical_start..];
        b = &b[a_numerical_start..];

        let a_numerical_end = a
            .iter()
            .position(|c| !c.is_ascii_digit())
            .unwrap_or(a.len());
        let b_numerical_end = b
            .iter()
            .position(|c| !c.is_ascii_digit())
            .unwrap_or(b.len());

        let a_str = &a[a.iter().position(|&c| c != b'0').unwrap_or(a.len())
            ..a_numerical_end];
        let b_str = &b[b.iter().position(|&c| c != b'0').unwrap_or(b.len())
            ..b_numerical_end];

        match a_str.len().cmp(&b_str.len()) {
            Ordering::Equal => {}
            ord => return ord,
        }

        match a_str.cmp(b_str) {
            Ordering::Equal => {}
            ord => return ord,
        }

        a = &a[a_numerical_end..];
        b = &b[b_numerical_end..];
    }

    Ordering::Equal
}
