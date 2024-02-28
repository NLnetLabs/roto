use log::trace;
use roto::compiler::{compile, Compiler};

use roto::blocks::Scope;
use roto::types::collections::{ElementTypeValue, List, Record};
use roto::types::datasources::DataSource;
use roto::types::typedef::TypeDef;
use roto::types::typevalue::TypeValue;
use roto::vm;

use routecore::asn::Asn;
use routecore::bgp::communities::HumanReadableCommunity as Community;

mod common;

fn test_data(
    name: Scope,
    source_code: &'static str,
) -> Result<(), Box<dyn std::error::Error>> {
    println!("Evaluate filter-map {}...", name);

    // Type coercion doesn't work here...
    let filter_map_arguments =
        vec![("extra_asn", TypeValue::from(Asn::from(65534_u32)))];

    let mut c = Compiler::new();
    c.with_arguments(&name, filter_map_arguments)?;

    let compile_res = c.build_from_compiler(source_code);

    if let Err(e) = &compile_res {
        eprintln!("{e}");
    }

    let roto_packs = compile_res?;

    let mut roto_pack = roto_packs.retrieve_pack_as_refs(&name)?;
    let _count: TypeValue = 1_u32.into();
    let prefix: TypeValue =
        routecore::addr::Prefix::new("193.0.0.0".parse().unwrap(), 24)?
            .into();
    let next_hop: TypeValue =
        std::net::IpAddr::V4(std::net::Ipv4Addr::new(193, 0, 0, 23)).into();
    let as_path = vec![Asn::from_u32(1)].into();
    let asn: TypeValue = Asn::from_u32(211321).into();

    println!("{:?}", asn);

    let comms =
        TypeValue::List(List::new(vec![ElementTypeValue::Primitive(
            Community::from([127, 12, 13, 12]).into(),
        )]));

    // let my_comms_type =
    //     TypeDef::List(Box::new(TypeDef::List(Box::new(TypeDef::Community))));

    let my_comms_type = (&comms).into();

    let my_nested_rec_type =
        TypeDef::new_record_type(vec![("counter", Box::new(TypeDef::U32))])
            .unwrap();

    let _my_nested_rec_instance =
        Record::create_instance_with_ordered_fields(
            &my_nested_rec_type,
            vec![("counter", 1_u32.into())],
        )
        .unwrap();

    let my_rec_type = TypeDef::new_record_type(vec![
        ("prefix", Box::new(TypeDef::Prefix)),
        ("as-path", Box::new(TypeDef::AsPath)),
        ("origin", Box::new(TypeDef::Asn)),
        ("next-hop", Box::new(TypeDef::IpAddr)),
        ("med", Box::new(TypeDef::U32)),
        ("local-pref", Box::new(TypeDef::U32)),
        ("communities", Box::new(my_comms_type)),
    ])
    .unwrap();

    let my_payload = Record::create_instance_with_ordered_fields(
        &my_rec_type,
        vec![
            ("prefix", prefix),
            ("as-path", as_path),
            ("origin", asn),
            ("next-hop", next_hop),
            ("med", 80_u32.into()),
            ("local-pref", 20_u32.into()),
            ("communities", comms),
        ],
    )
    .unwrap();

    trace!("PAYLOAD {:#?}", my_payload);

    let source_asns_type =
        TypeDef::new_record_type(vec![("asn", Box::new(TypeDef::Asn))])?;
    let new_sa_rec = Record::create_instance_with_ordered_fields(
        &source_asns_type,
        vec![("asn", Asn::from_u32(300).into())],
    )?;

    let mem = &mut vm::LinearMemory::uninit();

    println!("Used Arguments");
    println!("{:#?}", &roto_pack.arguments);
    println!("Used Data Sources");
    println!("{:#?}", &roto_pack.data_sources);

    // table source_asns contains AsnLines {
    //     asn: Asn
    // }
    let sources_asns =
        DataSource::table_from_records("source_asns", vec![new_sa_rec])?;
    roto_pack.set_source(sources_asns)?;

    for mb in roto_pack.get_mir().iter() {
        println!("{}", mb);
    }

    let mut vm = vm::VmBuilder::new()
        // .with_arguments(args)
        .with_data_sources(roto_pack.data_sources)
        .with_mir_code(roto_pack.mir)
        .build()?;

    let res = vm.exec(my_payload, None::<Record>, None, mem).unwrap();

    println!("\nRESULT");
    println!("action: {}", res.accept_reject);
    println!("rx    : {:?}", res.rx);
    println!("tx    : {:?}", res.tx);

    Ok(())
}

#[test]
fn test_filter_map_1() {
    common::init();

    test_data(
        Scope::FilterMap("in-filter-map".into()),
        r###"
            filter-map in-filter-map with my_asn: Asn {
                define for ext_r: ExtRoute with extra_asn: Asn {
                    // specify the types of that this filter receives
                    // and sends.
                    // rx_tx route: StreamRoute;
                    rx route: MyPayload;
                    tx ext_route: Route;

                    // specify additional external data sets that will be consulted.
                    // use table source_asns;

                    // assignments
                    extra_in_table = source_asns.contains(extra_asn); // 0
                    route_in_table = source_asns.contains(route.as-path.origin()); // 1
                    
                    // this is aliasing, kinda' useless, but hey, it's allowed
                    my_source = source_asns; // 2

                    // Some literals. Literals are turned into constants, but
                    // they can be converted to other types.
                    prefix_len = 24; // 3
                    ROV_INVALID_AS = 0xFFFFFF010; // 4
                    some_bool = false; // 5
                    
                    // syntactically valid, but doesn't exist.
                    // my_basic_call = builtin_func();

                    // assignment to a field from an argument
                    my_route_path = route.as-path; // 6
                    
                    // prefix_len triggers a type conversion from IntegerLiteral to PrefixLength
                    fixed_len_prefix = Prefix.from(route.prefix.address(), prefix_len); // 7

                    found_prefix_pref = 100;

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
                        fixed_len_prefix.len() == prefix_len;
                        extra_in_table;
                        route_in_table;
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
                    }
                }
               
                action set-best {
                   // This shouldn't be allowed, a filter does not get to
                   // decide where to write.
                   // Doesn't work either, users can only modify the rx type of a filter-map.
                   // route_in_table.set(true); 
                   // This should work. The filter is allowed to modify the
                   // route that flows through it.
                   route.local-pref.set(200);
                //    route.origin.set(AS300);
                }

                action set-rov-invalid-asn-community {
                    // route.communities.push(ROV_INVALID_AS);
                    route.local-pref.set(50);
                }

                apply {
                    // use best-path;
                    filter exactly-one rov-valid matching { 
                        set-best; 
                        set-rov-invalid-asn-community; 
                        return accept; 
                    };
                    // use backup-path;
                    filter match on-my-terms matching { set-best; return accept; };
                    // use backup-path;
                    filter match on-my-terms not matching { 
                        set-rov-invalid-asn-community; 
                        return reject;
                    };
                    return accept;
                }
            }

            table source_asns contains AsnLines { 
                asn: Asn
            }

            type MyPayload {
                prefix: Prefix,
                as-path: AsPath,
                origin: Asn,
                next-hop: IpAddress,
                med: U32,
                communities: [Community],
                local-pref: U32
            }
        "###,
    ).unwrap();
}
