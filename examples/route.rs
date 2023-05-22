use roto::compile::Compiler;

use roto::types::builtin::{RawRouteWithDeltas, RotondaId, UpdateMessage, Prefix};
use roto::types::collections::Record;
use roto::types::typevalue::TypeValue;
use roto::vm;
use routecore::bgp::message::SessionConfig;

fn test_data(
    name: &str,
    source_code: &'static str,
) -> Result<(), Box<dyn std::error::Error>> {
    println!("Evaluate module {}...", name);

    // Compile the source code in this example
    let rotolo = Compiler::build(source_code)?;
    let roto_pack = rotolo.retrieve_pack_as_arc(name)?;

    // BGP UPDATE message containing MP_REACH_NLRI path attribute,
    // comprising 5 IPv6 NLRIs
    let buf = bytes::Bytes::from(vec![
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x88, 0x02, 0x00, 0x00, 0x00,
        0x71, 0x80, 0x0e, 0x5a, 0x00, 0x02, 0x01, 0x20, 0xfc, 0x00, 0x00,
        0x10, 0x00, 0x01, 0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x10, 0xfe, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x00, 0x80, 0xfc, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x10, 0x40, 0x20, 0x01, 0x0d, 0xb8, 0xff, 0xff, 0x00,
        0x00, 0x40, 0x20, 0x01, 0x0d, 0xb8, 0xff, 0xff, 0x00, 0x01, 0x40,
        0x20, 0x01, 0x0d, 0xb8, 0xff, 0xff, 0x00, 0x02, 0x40, 0x20, 0x01,
        0x0d, 0xb8, 0xff, 0xff, 0x00, 0x03, 0x40, 0x01, 0x01, 0x00, 0x40,
        0x02, 0x06, 0x02, 0x01, 0x00, 0x00, 0x00, 0xc8, 0x80, 0x04, 0x04,
        0x00, 0x00, 0x00, 0x00,
    ]);

    let update: UpdateMessage =
        UpdateMessage::new(buf, SessionConfig::modern());
    let prefixes: Vec<Prefix> =
            update.0.nlris().iter().filter_map(|n| n.prefix().map(|p| p.into())).collect();
    let msg_id = (RotondaId(0), 0);

    let payload: RawRouteWithDeltas = RawRouteWithDeltas::new_with_message(
        msg_id,
        prefixes[0],
        update,
    );

    // Create the VM
    println!("Used Arguments");
    println!("{:#?}", &roto_pack.arguments);
    println!("Used Data Sources");
    println!("{:#?}", &roto_pack.data_sources);

    let module_arguments = vec![(
        "extra_asn",
        // use Roto type coercion
        TypeValue::from(65534_u32)
    )];

    let ds_ref = roto_pack.data_sources.iter().collect::<Vec<_>>();
    let args = rotolo.compile_arguments(name, module_arguments)?;

    let mut vm = vm::VmBuilder::new()
        .with_arguments(args)
        .with_data_sources(ds_ref)
        .with_mir_code(roto_pack.mir)
        .build();

    let mem = &mut vm::LinearMemory::uninit();
    let res = vm.exec(
        payload,
        None::<Record>,
        // Some(module_arguments),
        None,
        mem,
    )
    .unwrap();

    println!("\nRESULT");
    println!("action: {}", res.0);
    println!("rx    : {:?}", res.1);
    println!("tx    : {:?}", res.2);

    Ok(())
}

fn main() {
    test_data(
        "in-module",
        r###"
            module in-module with my_asn: Asn {
                define for ext_r: ExtRoute with extra_asn: Asn {
                    // specify the types of that this filter receives
                    // and sends.
                    // rx_tx route: StreamRoute;
                    rx route: Route;
                    tx ext_route: ExtRoute;

                    // specify additional external data sets that will be consulted.
                    use table source_asns;
                    use rib rib-rov;

                    // assignments
                    extra_in_table = source_asns.contains(extra_asn); // 0
                    route_in_table = source_asns.contains(route.as-path.origin()); // 1
                    
                    // this is aliasing, kinda' useless, but hey, it's allowed
                    extra_extra = rib-extra.blixer.bla; // 2
                    my_source = source_asns; // 3

                    // Some literals. Literals are turned into constants, but
                    // they can be converted to other types.
                    prefix_len = 24; // 4
                    ROV_INVALID_AS = 0xFFFFFF010; // 5
                    some_bool = false; // 6

                    // also supported is calling a field beyond a method call:
                    found_prefix_pref = rib-rov.longest_match(route.prefix).local-pref; // 7

                    found_prefix = rib-rov.longest_match(route.prefix); // 8
                    
                    // syntactically valid, but doesn't exist.
                    // my_basic_call = builtin_func();

                    // assignment to a field from an argument
                    my_route_path = route.as-path; // 9
                    
                    // prefix_len triggers a type conversion from IntegerLiteral to PrefixLength
                    fixed_len_prefix = Prefix.from(route.prefix.address(), prefix_len); // 10

                    my_my_route_path = my_route_path;

                    // try to mess it up
                    // my_recursor = my_recursor_trasher;
                    // my_recursor_trasher = my_recursor;

                    // syntactically correct, but semantically wrong: these
                    // identifiers are not defined.
                    // bullshitter = a.b.c.d(x,y,z).e.f(o.p()).g;

                    my_false = false;
                }
            
                term rov-valid for route: Route {
                    match {
                        found_prefix_pref == route.local-pref;
                        my_route_path.origin() == found_prefix.as-path.origin();
                        extra_in_table;
                        fixed_len_prefix.len() == prefix_len;
                        route.as-path.origin() == found_prefix.as-path.origin();
                        (found_prefix.prefix.exists() && found_prefix.prefix.exists()) || route_in_table;
                        found_prefix.prefix.len() == 24;
                        route_in_table;
                        route.prefix.len() <= found_prefix.prefix.len();
                    }
                }

                // term var-test for route: Route {
                //     match {
                //         my_my_route_path;
                //         my_route_path;
                //     }
                // }

                term on-my-terms for route: Route {
                    match {
                        my_false;
                        //  rib-extra.contains(route.as-path.origin());
                        route.prefix.len() == 24;
                        route.as-path.origin() == found_prefix.as-path.origin();
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
                   route.local-pref.set(200);
                   // route.origin.set(AS300);
                }

                action set-rov-invalid-asn-community {
                    route.communities.push(ROV_INVALID_AS);
                }

                apply {
                    use best-path;
                    filter exactly-one rov-valid matching { 
                        set-best; 
                        set-rov-invalid-asn-community; 
                        return accept; 
                    };
                    use backup-path;
                    filter match on-my-terms matching { set-best; return accept; };
                    use backup-path;
                    filter match on-my-terms not matching { 
                        set-rov-invalid-asn-community; 
                        return reject;
                    };
                    return accept;
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

            table source_asns contains AsnLines { 
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
    ).unwrap();
}
