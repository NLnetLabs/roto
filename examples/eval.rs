use std::cell::RefCell;
use std::collections::HashMap;
use roto::symbols::GlobalSymbolTable;

use nom::error::convert_error;
use roto::ast::*;
use roto::symbols::{Scope, SymbolTable};

fn test_data(name: &str, data: &'static str, expect_success: bool) -> Result<(), Box<dyn std::error::Error>> {
    println!("eval test {}", name);
    let parsed_data = Root::parse_str(data);
    // println!("{} {:#?}", name, parsed_data);
    if let Err(e) = parsed_data.clone() {
        println!("{}", convert_error(data, e));
    }

    match expect_success {
        false => assert!(parsed_data.is_err()),
        true => assert!(parsed_data.is_ok()),
    }

    let symbols = HashMap::<Scope, SymbolTable>::new();
    let eval = parsed_data?;
    
    let symbols = RefCell::new(symbols);
    let symbols = GlobalSymbolTable::new(symbols);
    let ev2 = eval.1.eval(symbols.clone());

    println!("{:#?}", symbols);
    println!("{:?} {:#?}", name, eval);
   
    ev2
}

fn main() {
    test_data(
        "module_1",
        r###"
            module in-module with my_asn: Asn {
               define for ext_r: ExtRoute with extra_asn: Asn {
                  // specify the types of that this filter receives
                  // and sends.
                  // rx_tx route: StreamRoute;
                  rx route: StreamRoute;
                  tx ext_route: ExtRoute;

                  // specify additional external data sets that will be consulted.
                  use table source_asns;
                  use rib rib-rov;

                  // assignments
                  route_in_table = source_asns.contains(extra_asn, route.as-path.origin());

                  // specify another RIB that is used in this filter.
                  found_prefix = rib-rov.longest_match(route.prefix);
                  fixed_len_prefix = Prefix.from(route.prefix.address(), 24); // maybe /24
               }
            
               term rov-valid for route: Route {
                    match {
                        (found_prefix.matches && found_prefix.slices) || route_in_table.exists;
                        found_prefix.len == 24;
                        route.prefix.len <= found_prefix.max_len;
                        route.asn.bgp.origin-asn == found_prefix.asn;
                    }
                }
               
               action set-best for route: Route {
                   rib-rov.set-best(route);
                   route.local_pref.set(200);
                   route.origin.set(extra_asn);
               }

               apply {
                    use best-path;
                    filter exactly-one exists(found_prefix) matching { set-best(route); return accept; };
                    use backup-path;
                    filter match rov-invalid-asn matching { set-rov-invalid-asn-community; return reject; };
               }
            }

            // comment
            rib rib-extra contains ExtRoute { 
                blaffer: U32, 
                blooper: Prefix,
                blixer: { 
                    bla: U8, 
                    salt: { 
                        pp: Prefix 
                    } 
                }  
            }

            table source_asns contains Asn { 
                asn: Asn
            }

            // yo, rib
            rib rib-rov contains StreamRoute {
                prefix: Prefix, // this is shit: it's the key
                as-path: AsPath,
                origin: Asn,
                next-hop: IpAddress,
                med: U32,
                local-pref: U32,
                community: [Community]
            }
        "###,
        true,
    ).unwrap();
}