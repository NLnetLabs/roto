use nom::error::convert_error;
use roto::ast::*;

fn main() {
    let random_crap = Root::parse_str("##@#@#kdflfk!  abc  \n  ");
    println!("random crap {:?}", random_crap);
    assert!(random_crap.is_err());
    // assert_eq!(random_crap.unwrap().fragment(), &"##@#@#kdflfk!  abc  \n   ");
    let random_comment = Root::parse_str("// @@##This is a comment##@@!!! \n");
    assert!(random_comment.is_err());
    // println!(
    //     "random comment leftover {}",
    //     random_comment.clone().unwrap().fragment()
    // );
    // assert!(random_comment.unwrap().fragment().is_empty());

    let test_rib = Root::parse_str("rib my_rib {}\n// comment\n");
    println!("test_rib {:?}", test_rib);
    assert!(test_rib.is_ok());

    let test_rib2 = Root::parse_str("rib [] ribribribmodule\n");
    println!("test_rib2 {:?}", test_rib2);
    if let Err(e) = test_rib2.clone() {
        println!("test_root {}", e);
    }
    assert!(test_rib2.is_err());

    let test_rib3 = Root::parse_str("rib my_rib3 { bla: Bla, bloo: Bloo }\n// comment\n");
    println!("test_rib3 {:?}", test_rib3);
    assert!(test_rib3.is_ok());

    let test_rib4 = Root::parse_str("rib my_rib4 { bla: Bla, bloo: Bloo, }\n// comment\n");
    println!("test_rib4 {:?}", test_rib4);
    assert!(test_rib4.is_ok());

    let test_rib5 = Root::parse_str("// comment\nrib my_rib5 { bla: Bla, blow_up }\n");
    println!("test_rib5 {:#?}", test_rib5);
    if let Err(e) = test_rib5.clone() {
        println!("test_root {}", e);
    }
    assert!(test_rib5.is_err());
    // assert_eq!(test_rib5.unwrap_err().pos.col, 25);

    let test_rib_without_a_name = Root::parse_str("// comment\nrib {}\n");
    println!("test_rib_without_a_name {:#?}", test_rib_without_a_name);
    if let Err(e) = test_rib_without_a_name.clone() {
        println!("test_root {}", e);
    }
    assert!(test_rib_without_a_name.is_err());

    let data = "// bla bla \n// more comments\n";
    let test_root = Root::parse_str(data);
    if let Err(e) = test_root.clone() {
        println!("test_root {}", e);
    }
    assert!(test_root.is_ok());
}
