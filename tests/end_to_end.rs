use log::trace;

use roto::ast::AcceptReject;
use roto::compiler::Compiler;
use roto::blocks::Scope::{self, Filter, FilterMap};
use roto::types::collections::{ElementTypeValue, List, Record};
use roto::types::datasources::{DataSource, Rib};
use roto::types::typedef::TypeDef;
use roto::types::typevalue::TypeValue;
use roto::vm::{self, VmResult};
use rotonda_store::prelude::MergeUpdate;
use routecore::bgp::communities::HumanReadableCommunity as Community;
use routecore::asn::Asn;

mod common;

#[derive(Debug, Clone)]
struct RibValue(Vec<TypeValue>);

impl MergeUpdate for RibValue {
    type UserDataIn = ();
    type UserDataOut = ();

    fn merge_update(
        &mut self,
        update_record: RibValue,
        _: Option<&Self::UserDataIn>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        self.0 = update_record.0;
        Ok(())
    }

    fn clone_merge_update(
        &self,
        update_meta: &Self,
        _: Option<&Self::UserDataIn>,
    ) -> Result<(Self, Self::UserDataOut), Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized,
    {
        let mut new_meta = update_meta.0.clone();
        new_meta.push(self.0[0].clone());
        Ok((RibValue(new_meta), ()))
    }
}

impl std::fmt::Display for RibValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

fn test_data(
    name: Scope,
    source_code: &'static str,
    filter_args: Vec<(&str, TypeValue)>,
    data_sources: Vec<DataSource>,
) -> Result<VmResult, Box<dyn std::error::Error>> {
    trace!("Evaluate filter-map {}...", name);

    let mut c = Compiler::new();
    c.with_arguments(&name, filter_args)?;
    let compiler_res = c.build_from_compiler(source_code);

    if let Err(e) = &compiler_res {
        eprintln!("{e}");
    }

    let roto_packs = compiler_res?;

    let mut roto_pack = roto_packs.retrieve_pack_as_refs(&name)?;
    let _count: TypeValue = 1_u32.into();
    let prefix: TypeValue =
        routecore::addr::Prefix::new("193.0.0.0".parse().unwrap(), 24)?
            .into();
    let next_hop: TypeValue =
        std::net::IpAddr::V4(std::net::Ipv4Addr::new(193, 0, 0, 23)).into();
    let as_path = vec![Asn::from_u32(1)].into();
    let asn: TypeValue = Asn::from_u32(211321).into();

    trace!("ASN {:?}", asn);

    let comms_list = List::new(vec![ElementTypeValue::Primitive(
        Community::from([
            127, 12, 13, 12,
        ]).into(),
    )]);

    trace!("comms list {}", comms_list);

    let my_nested_rec_type =
        TypeDef::new_record_type(vec![("counter", Box::new(TypeDef::U32))])
            .unwrap();

    let _my_nested_rec_instance =
        Record::create_instance_with_ordered_fields(
            &my_nested_rec_type,
            vec![("counter", 1_u32.into())],
        )
        .unwrap();

    let comms =
        TypeValue::List(List::new(vec![ElementTypeValue::Primitive(
            Community::from([
                127, 12, 13, 12,
            ]).into()),
        ]));

    trace!("comms instance {}", comms);

    let my_comms_type: TypeDef = (&comms).into();

    trace!("comms type {}", my_comms_type);

    let my_rec_type = TypeDef::new_record_type(vec![
        ("prefix", Box::new(TypeDef::Prefix)),
        ("as-path", Box::new(TypeDef::AsPath)),
        ("origin", Box::new(TypeDef::Asn)),
        ("next-hop", Box::new(TypeDef::IpAddr)),
        ("med", Box::new(TypeDef::U32)),
        ("local-pref", Box::new(TypeDef::U32)),
        ("community", Box::new(my_comms_type)),
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
            ("community", comms),
        ],
    )
    .unwrap();

    // Turn it into a DataSource.
    let mem = &mut vm::LinearMemory::uninit();

    trace!("Used Arguments");
    trace!("{:#?}", &roto_pack.arguments);
    trace!("Used Data Sources");
    trace!("{:#?}", &roto_pack.data_sources);

    for mb in roto_pack.get_mir().iter() {
        trace!("{}", mb);
    }

    for data_source in data_sources {
        roto_pack.set_source(data_source)?;
    }

    let mut vm = vm::VmBuilder::new()
        // .with_arguments(args)
        .with_data_sources(roto_pack.data_sources)
        .with_mir_code(roto_pack.mir)
        .build()?;

    let res = vm.exec(my_payload, None::<Record>, None, mem).unwrap();

    trace!("\nRESULT");
    trace!("action: {}", res.accept_reject);
    trace!("rx    : {:?}", res.rx);
    trace!("tx    : {:?}", res.tx);

    Ok(res)
}

#[test]
#[ignore]
fn test_filter_map_1() {
    common::init();

    // Type coercion doesn't work here...
    let filter_map_arguments =
        vec![("extra_asn", TypeValue::from(Asn::from(65534_u32)))];

    let source_asns_type =
        TypeDef::new_record_type(vec![("asn", Box::new(TypeDef::Asn))])
            .unwrap();
    let new_sa_rec = Record::create_instance_with_ordered_fields(
        &source_asns_type,
        vec![("asn", Asn::from_u32(300).into())],
    )
    .unwrap();
    let mut data_sources =
        vec![
            DataSource::table_from_records("source_asns", vec![new_sa_rec])
                .unwrap(),
        ];

    let comms =
        TypeValue::List(List::new(vec![ElementTypeValue::Primitive(
            Community::from([
                127, 12, 13, 12,
            ]).into()),
        ]));

    trace!("comms instance {}", comms);

    let my_comms_type: TypeDef = (&comms).into();

    trace!("comms type {}", my_comms_type);

    // rib record
    let my_rec_type = TypeDef::new_record_type(vec![
        ("prefix", Box::new(TypeDef::Prefix)),
        ("as-path", Box::new(TypeDef::AsPath)),
        ("origin", Box::new(TypeDef::Asn)),
        ("next-hop", Box::new(TypeDef::IpAddr)),
        ("med", Box::new(TypeDef::U32)),
        ("local-pref", Box::new(TypeDef::U32)),
        ("community", Box::new(my_comms_type)),
    ])
    .unwrap();

    // external rib
    data_sources.push(
        Rib::new(
            "rib-rov",
            my_rec_type,
            rotonda_store::MultiThreadedStore::<RibValue>::new().unwrap(),
        )
        .into(),
    );

    test_data(
        FilterMap("in-filter-map".into()),
        r###"
            filter-map in-filter-map with my_asn: Asn {
                define for ext_r: ExtRoute with extra_asn: Asn {
                    // specify the types of that this filter receives
                    // and sends.
                    // rx_tx route: StreamRoute;
                    rx route: StreamRoute;
                    tx ext_route: ExtRoute;

                    // specify additional external data sets that will be consulted.
                    // use table source_asns;
                    // use rib rib-rov;

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
                        fixed_len_prefix.len() == prefix_len;
                        route.origin == found_prefix.as-path.origin();
                        (found_prefix.prefix.exists() && found_prefix.prefix.exists()) || route_in_table;
                        found_prefix.prefix.len() == 24;
                        extra_in_table;
                        route_in_table;
                        // global-truth-method();
                        // last.truth;
                        // last();
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
                   // Doesn't work either, users can only modify the rx type of a filter-map.
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
                prefix: Prefix,
                as-path: AsPath,
                origin: Asn,
                next-hop: IpAddress,
                med: U32,
                local-pref: U32,
                community: [Community]
            }
        "###,
        filter_map_arguments,
        data_sources
    ).unwrap();
}

#[test]
fn test_filter_map_2() {
    common::init();
    let res = test_data(
        Filter("in-filter".into()),
        r#"
        filter in-filter {
            define {
                rx msg: Route;
            }
        
            term filter_asn {
                match {
                    // msg.as-path.origin() == AS65001;
                    1 == 1;
                }
            }
        
            apply {
                filter match filter_asn matching {
                    return accept;
                };
                accept;
            }
        }
        "#,
        vec![],
        vec![],
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn test_filter_map_3() {
    common::init();
    let res = test_data(
        Filter("in-filter".into()),
        r#"
        filter in-filter {
            define {
                rx msg: Route;
            }
        
            term filter_asn {
                match {
                    // msg.as-path.origin() == AS65001;
                    1 == 1;
                }
            }
        
            apply {
                filter match filter_asn matching {
                    return reject;
                };
                reject;
            }
        }
        "#,
        vec![],
        vec![],
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn test_reject_all_filter() {
    common::init();
    let res = test_data(
        Filter("reject-filter".into()),
        r#"
        filter reject-filter {
            define {
                rx msg: Route;
            }
        
            apply {
                reject;
            }
        }
        "#,
        vec![],
        vec![],
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn test_accept_all_filter() {
    common::init();
    let res = test_data(
        Filter("accept-filter".into()),
        r#"
        filter accept-filter {
            define {
                rx msg: Route;
            }
        
            apply {
                accept;
            }
        }
        "#,
        vec![],
        vec![],
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}
