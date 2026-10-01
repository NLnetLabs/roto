use roto::{Runtime, module::Parsed};

const DIR_PATH: &str = "./yang-models";

pub fn main() {
    // let entry_point = SourceFile::read(DIR_PATH.as_ref()).unwrap();
    let entry_file = std::path::Path::new(DIR_PATH).join("rotonda-main.yang");
    let parsed = Parsed::from_entry_point(
        entry_file.as_path(),
        std::path::Path::new(DIR_PATH),
    )
    .unwrap();
    // println!("modules {:#?}", parsed.module_tree.modules);

    let rt = Runtime::new();
    let t_checked = parsed.typecheck(&rt).unwrap();
    println!(
        "[declarations] {:#?}",
        t_checked
            .type_info
            .scope_graph
            .declarations
            .iter()
            .map(|dec| (dec.0.ident, &dec.1.kind, &dec.1.doc))
            .collect::<Vec<_>>()
    );

    let modules = &t_checked.module_tree.modules;
    println!(
        "module tree {:#?}",
        modules.iter().map(|m| m.ident.clone()).collect::<Vec<_>>()
    );

    println!("resolved name {:#?}", t_checked.type_info.resolved_names);
}
