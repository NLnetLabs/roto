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
    // println!("{:?} {:#?}", name, eval);
   
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
                    // extra_in_table = source_asns.contains(extra_asn);
                    route_in_table = source_asns.contains(route.as-path.origin());
                    ROV_INVALID_AS = 0xFFFFFF010;

                    // specify another RIB that is used in this filter.
                    found_prefix = rib-rov.longest_match(route.prefix);
                    fixed_len_prefix = Prefix.from(route.prefix.address(), /24);
                }
            
                term rov-valid for route: Route {
                    match {
                        route.origin == found_prefix.as-path.origin();
                        (found_prefix.prefix.matches() && found_prefix.prefix.matches()) || route_in_table;
                        found_prefix.prefix.len() == 24;
                        route_in_table;
                        route.prefix.len() <= found_prefix.prefix.len();
                    }
                }

                term on-my-terms for route: Route {
                    match {
                        route.prefix.len() == 24;
                        route.as-path == found_prefix.as-path.origin();
                    }
                }
               
                action set-best {
                   // This shouldn't be allowed, a filter does not get to
                   // decide where to write.
                   // rib-rov.set-best(route);
                   // Doesn't work either, users can only modify the rx type of a module.
                   // route_in_table.set(true); 
                   // This should work. The filter is allowed to modify the
                   // route that flows through it.
                   route.set(local-pref, 200);
                   route.set(origin, extra_asn);
                }

                action set-rov-invalid-asn-community {
                    route.set(community, ROV_INVALID_AS);
                }

                apply {
                    use best-path;
                    filter exactly-one rov-valid matching { 
                        set-best; 
                        set-rov-invalid-asn-community; 
                        return accept; 
                    };
                    use backup-path;
                    filter match on-my-terms matching { set-rov-invalid-asn-community; return reject; };
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