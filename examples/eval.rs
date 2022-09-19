use std::collections::HashMap;

use nom::error::convert_error;
use roto::ast::*;
use roto::symbols::SymbolTable;

fn test_data<'a>(name: &str, data: &'static str, expect_success: bool) -> Result<(), Box<dyn std::error::Error>> {
    println!("test {}", name);
    let parsed_data = Root::parse_str(data);
    println!("{} {:#?}", name, parsed_data);
    if let Err(e) = parsed_data.clone() {
        println!("{}", convert_error(data, e));
    }

    match expect_success {
        false => assert!(parsed_data.is_err()),
        true => assert!(parsed_data.is_ok()),
    }

    let symbols = &mut HashMap::<ShortString, SymbolTable>::new();
    let eval = parsed_data?;
    
    let ev2 = eval.1.eval(symbols);

    println!("{:?} {:#?}", name, eval);
    println!("{:#?}", symbols);
   
    ev2
}

fn main() {
    test_data(
        "module_with_apply_2",
        r###"
            module my_module for my-rib with bla: Blaffer {
               define {
                   use bla;
                   bla = bla();
               }
            
               term blaffer_filter {
                   match { 
                        blaffer.blaf.contains(something,"somewhat") > blaf();
                        ( bla.bla() > some_external_set );
                   }
               }
               
               action blaffer {
                   blaffer.blaf(bla);
               }

               apply {
                    use best-path;
                    filter exactly-one exists(found_prefix) matching { set-best(route); return accept; };
                    use backup-path;
                    filter match rov-invalid-asn matching { set-rov-invalid-asn-community; return reject; };
               }
            }
            // comment
            rib unrib contains Blaffer { 
                blaffer: U32, 
                blooper: Prefix, 
                blixer: { 
                    bla: U8, 
                    salt: { 
                        pp: Prefix 
                    } 
                }  
            }
        "###,
        true,
    ).unwrap();
}