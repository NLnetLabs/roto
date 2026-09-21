use roto::{FileTree, Runtime};

const DIR_PATH: &str = "./yang-models";

pub fn main() {
    let tree = FileTree::read_yang(DIR_PATH).unwrap();
    // println!(
    //     "file tree {:#?}",
    //     tree.files.iter().map(|t| &t.name).collect::<Vec<_>>()
    // );
    let parsed = tree.parse_with_modules().unwrap();
    // println!("modules {:#?}", parsed.module_tree.modules);

    // let modules = &parsed.module_tree.modules;
    let rt = Runtime::new();
    let t_checked = parsed.typecheck(&rt).unwrap();
    println!("type_info {:?}", t_checked.type_info.types);
    // t_checked.lower_to_mir();

    let modules = &t_checked.module_tree.modules;
    println!(
        "module tree {:#?}",
        modules.iter().map(|m| m.ident.clone()).collect::<Vec<_>>()
    );

    println!("resolved name {:#?}", t_checked.type_info.resolved_names);
}
