use std::cell::RefCell;
use std::rc::Rc;

use roto::compile::compile;
use roto::symbols::GlobalSymbolTable;

use nom::error::convert_error;
use roto::ast::*;
use roto::types::builtin::{
    AsPath, Asn, BuiltinTypeValue, Community, CommunityType, U32,
};
use roto::types::collections::{ElementTypeValue, List, Record};
use roto::types::typedef::TypeDef;
use roto::types::typevalue::TypeValue;
use roto::vm;
use roto::vm::ArgumentsMap;

fn test_data(
    name: &str,
    data: &'static str,
    expect_success: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    println!("eval test {}", name);
    let parsed_data = Root::parse_str(data);
    if let Err(e) = parsed_data.clone() {
        println!("{}", convert_error(data, e));
    }

    match expect_success {
        false => assert!(parsed_data.is_err()),
        true => assert!(parsed_data.is_ok()),
    }

    let eval = parsed_data?;

    let symbols = GlobalSymbolTable::new();
    let ev2 = eval.1.eval(symbols.clone());

    println!("{:#?}", symbols);

    let roto_pack = compile(symbols)?;

    let count =
        BuiltinTypeValue::create_instance(TypeDef::U32, 1_u32).unwrap();

    let count2 = BuiltinTypeValue::create_instance(
        TypeDef::Prefix,
        routecore::addr::Prefix::new("193.0.0.0".parse().unwrap(), 24)
            .unwrap(),
    )
    .unwrap();

    let ip_address = BuiltinTypeValue::create_instance(
        TypeDef::IpAddress,
        std::net::IpAddr::V4(std::net::Ipv4Addr::new(193, 0, 0, 23)),
    )
    .unwrap();

    let as_path = BuiltinTypeValue::create_instance(
        TypeDef::AsPath,
        BuiltinTypeValue::AsPath(
            AsPath::new(vec![routecore::asn::Asn::from_u32(1)]).unwrap(),
        ),
    )
    .unwrap();

    let asn = BuiltinTypeValue::create_instance(
        TypeDef::Asn,
        Asn::from_u32(211321),
    )
    .unwrap();
    println!("{:?}", asn);

    let comms =
        TypeValue::List(List::new(vec![ElementTypeValue::Primitive(
            BuiltinTypeValue::Community(Community::new(
                CommunityType::Standard,
            )),
        )]));

    let my_comms_type =
        TypeDef::List(Box::new(TypeDef::List(Box::new(TypeDef::Community))));

    let my_nested_rec_type =
        TypeDef::new_record_type(vec![("counter", Box::new(TypeDef::U32))])
            .unwrap();

    let my_nested_rec_instance = Record::create_instance(
        &my_nested_rec_type,
        vec![(
            "counter",
            TypeValue::Builtin(BuiltinTypeValue::U32(U32::new(1))),
        )],
    )
    .unwrap();

    let my_rec_type = TypeDef::new_record_type(vec![
        ("count", Box::new(TypeDef::U32)),
        ("count2", Box::new(TypeDef::Prefix)),
        ("ip_address", Box::new(TypeDef::IpAddress)),
        ("asn", Box::new(TypeDef::Asn)),
        ("as_path", Box::new(TypeDef::AsPath)),
        ("communities", Box::new(my_comms_type)),
        ("record", Box::new(my_nested_rec_type)),
    ])
    .unwrap();

    let my_payload = Record::create_instance(
        &my_rec_type,
        vec![
            ("count", count),
            ("count2", count2),
            ("ip_address", ip_address),
            ("asn", asn),
            ("as_path", as_path),
            ("communities", comms),
            ("record", TypeValue::Record(my_nested_rec_instance)),
        ],
    )
    .unwrap();

    let mem = vm::LinearMemory::new();
    let vars = vm::VariablesMap::new();
    let vars = RefCell::new(vars);

    println!("Arguments");
    println!("{:#?}", &roto_pack.arguments);
    println!("Data Sources");
    println!("{:#?}", &roto_pack.data_sources);

    let mut arguments = ArgumentsMap::new();

    arguments.insert(
        2,
        TypeValue::Builtin(BuiltinTypeValue::Asn(Asn::new(65534.into()))),
    );

    let mut vm = vm::VmBuilder::new()
        // .with_arguments(arguments)
        .build(&vars);

    vm.exec(
        my_payload,
        None::<Record>,
        Rc::new(arguments),
        RefCell::new(mem),
        roto_pack.mir,
    )
    .unwrap();

    ev2

    // Ok(())
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
                    found_prefix_prefix = rib-rov.longest_match(route.prefix).prefix; // 7

                    found_prefix = rib-rov.longest_match(route.prefix); // 8
                    
                    // syntactically valid, but doesn't exist.
                    // my_basic_call = builtin_func();

                    // assignment to a field from an argument
                    my_route_path = route.as-path; // 9
                    
                    // prefix_len triggers a type conversion from IntegerLiteral to PrefixLength
                    fixed_len_prefix = Prefix.from(route.prefix.address(), prefix_len); // 10

                    // try to mess it up
                    // my_recursor = my_recursor_trasher;
                    // my_recursor_trasher = my_recursor;

                    // syntactically correct, but semantically wrong: these
                    // identifiers are not defined.
                    // bullshitter = a.b.c.d(x,y,z).e.f(o.p()).g;
                }
            
                term rov-valid for route: Route {
                    match {
                        fixed_len_prefix == prefix_len;
                        route.origin == found_prefix.as-path.origin();
                        found_prefix.as-path.origin() == my_route_path.origin();
                        (found_prefix.prefix.exists() && found_prefix.prefix.exists()) || route_in_table;
                        found_prefix.prefix.len() == 24;
                        route_in_table;
                        route.prefix.len() <= found_prefix.prefix.len();
                    }
                }

                term on-my-terms for route: Route {
                    match {
                        rib-extra.contains(route.as-path.origin());
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
                   route.origin.set(AS300);
                }

                action set-rov-invalid-asn-community {
                    route.community.push(ROV_INVALID_AS);
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
        true,
    ).unwrap();
}
