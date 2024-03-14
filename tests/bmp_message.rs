use log::trace;

use roto::{
    ast::AcceptReject,
    blocks::Scope::{self, Filter, FilterMap},
    compiler::{CompileError, Compiler},
    types::{
        builtin::{
            BuiltinTypeValue, BytesRecord, NlriStatus, PeerId, PeerRibType,
            Provenance, RouteContext,
        },
        collections::Record,
        lazyrecord_types::{
            BmpMessage, InitiationMessage, LazyRecordTypeDef, RouteMonitoring,
        },
        typedef::TypeDef,
        typevalue::TypeValue,
    },
    vm::{self, VmResult},
};
use routecore::asn::Asn;
use routes::bmp::encode::{
    mk_peer_down_notification_msg, mk_per_peer_header, mk_termination_msg,
};

mod common;

fn test_data(
    name: Scope,
    source_code: &'static str,
    buf: Vec<u8>,
) -> Result<VmResult, Box<dyn std::error::Error>> {
    println!("Evaluate filter-map {}...", name);

    // Compile the source code in this example
    let rotolo = Compiler::build(source_code)?;
    let roto_pack = rotolo.retrieve_pack_as_refs(&name)?;

    let rm_msg = BytesRecord::<RouteMonitoring>::new(buf.clone().into());
    assert!(rm_msg.is_ok());
    let rm_msg = rm_msg.unwrap();
    let payload = TypeValue::Builtin(
        roto::types::builtin::BuiltinTypeValue::BmpRouteMonitoringMessage(
            rm_msg,
        ),
    );

    trace!(
        "BUF {}",
        routecore::bmp::message::Message::from_octets(buf.clone()).unwrap()
    );
    trace!("Used Arguments");
    trace!("{:#?}", &roto_pack.get_arguments());
    trace!("Used Data Sources");
    trace!("{:#?}", &roto_pack.get_data_sources());

    let ds_ref = roto_pack.get_data_sources();

    for mb in roto_pack.get_mir().iter() {
        println!("{}", mb);
    }

    let peer_ip = "192.0.2.0".parse().unwrap();

    let provenance = Provenance {
        timestamp: chrono::Utc::now(),
        connection_id: "192.0.2.0:178".parse().unwrap(),
        peer_id: PeerId {
            addr: peer_ip,
            asn: Asn::from(65534),
        },
        peer_bgp_id: [0; 4].into(),
        peer_distuingisher: [0; 8],
        peer_rib_type: PeerRibType::OutPost,
    };

    let context =
        RouteContext::new(None, NlriStatus::InConvergence, provenance);

    let mut vm = vm::VmBuilder::new()
        // .with_arguments(args)
        .with_context(context)
        .with_data_sources(ds_ref)
        .with_mir_code(roto_pack.get_mir())
        .build()?;

    let mem = &mut vm::LinearMemory::uninit();
    let res = vm
        .exec(
            payload,
            None::<Record>,
            // Some(filter_map_arguments),
            None,
            mem,
        )
        .unwrap();

    trace!("\nRESULT");
    trace!("action: {}", res.accept_reject);
    trace!("rx    : {:?}", res.rx);
    trace!("tx    : {:?}", res.tx);

    Ok(res)
}

fn test_data_2(
    name: Scope,
    source_code: &'static str,
) -> Result<VmResult, Box<dyn std::error::Error>> {
    println!("Evaluate filter-map {}...", name);

    // Compile the source code in this example
    let rotolo = Compiler::build(source_code)?;
    let roto_pack = rotolo.retrieve_pack_as_refs(&name)?;

    let buf = vec![
        0x03, 0x00, 0x00, 0x00, 0x67, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0a, 0xff, 0x00, 0x65, 0x00,
        0x01, 0x00, 0x00, 0x0a, 0x0a, 0x0a, 0x01, 0x54, 0xa2, 0x0e, 0x0c,
        0x00, 0x0e, 0x81, 0x09, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x37,
        0x02, 0x00, 0x00, 0x00, 0x1b, 0x40, 0x01, 0x01, 0x00, 0x40, 0x02,
        0x06, 0x02, 0x01, 0x00, 0x01, 0x00, 0x00, 0x40, 0x03, 0x04, 0x0a,
        0xff, 0x00, 0x65, 0x80, 0x04, 0x04, 0x00, 0x00, 0x00, 0x01, 0x20,
        0x0a, 0x0a, 0x0a, 0x02,
    ];

    // let bla = BmpMessage::from_octets(buf.into()).unwrap();
    let rm_msg = BytesRecord::<BmpMessage>::new(buf.into());
    assert!(rm_msg.is_ok());
    let rm_msg = rm_msg.unwrap();
    let payload = TypeValue::Builtin(
        roto::types::builtin::BuiltinTypeValue::BmpMessage(rm_msg),
    );

    trace!("Used Arguments");
    trace!("{:#?}", &roto_pack.get_arguments());
    trace!("Used Data Sources");
    trace!("{:#?}", &roto_pack.get_data_sources());

    let ds_ref = roto_pack.get_data_sources();

    for mb in roto_pack.get_mir().iter() {
        println!("{}", mb);
    }

    let peer_ip = "192.0.2.0".parse().unwrap();

    let provenance = Provenance {
        timestamp: chrono::Utc::now(),
        connection_id: "192.0.2.10:178".parse().unwrap(),
        peer_id: PeerId {
            addr: peer_ip,
            asn: Asn::from(65534),
        },
        peer_bgp_id: [0; 4].into(),
        peer_distuingisher: [0; 8],
        peer_rib_type: PeerRibType::OutPost,
    };

    let context =
        RouteContext::new(None, NlriStatus::InConvergence, provenance);

    let mut vm = vm::VmBuilder::new()
        // .with_arguments(args)
        .with_context(context)
        .with_data_sources(ds_ref)
        .with_mir_code(roto_pack.get_mir())
        .build()?;

    let mem = &mut vm::LinearMemory::uninit();
    let res = vm
        .exec(
            payload,
            None::<Record>,
            // Some(filter_map_arguments),
            None,
            mem,
        )
        .unwrap();

    trace!("\nRESULT");
    trace!("action: {}", res.accept_reject);
    trace!("rx    : {:?}", res.rx);
    trace!("tx    : {:?}", res.tx);

    Ok(res)
}

fn test_data_3(
    name: Scope,
    source_code: &'static str,
) -> Result<VmResult, Box<dyn std::error::Error>> {
    println!("Evaluate filter-map {}...", name);

    // Compile the source code in this example
    let rotolo = Compiler::build(source_code)?;
    let roto_pack = rotolo.retrieve_pack_as_refs(&name)?;

    // BMP PeerDownNotification type 3, containing a BGP NOTIFICATION.
    let buf = vec![
        0x03, 0x00, 0x00, 0x00, 0x46, 0x02, 0x00, 0x80, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x20, 0x01, 0x0d, 0xb8, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00,
        0x01, 0x00, 0x00, 0x0a, 0x00, 0x00, 0x0a, 0x62, 0x2d, 0xea, 0x80,
        0x00, 0x05, 0x58, 0x22, 0x03, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00,
        0x15, 0x03, 0x06, 0x02,
    ];

    // let bla = BmpMessage::from_octets(buf.into()).unwrap();
    let rm_msg = BytesRecord::<BmpMessage>::new(buf.into());
    assert!(rm_msg.is_ok());
    let rm_msg = rm_msg.unwrap();
    let payload = TypeValue::Builtin(
        roto::types::builtin::BuiltinTypeValue::BmpMessage(rm_msg),
    );

    trace!("Used Arguments");
    trace!("{:#?}", &roto_pack.get_arguments());
    trace!("Used Data Sources");
    trace!("{:#?}", &roto_pack.get_data_sources());

    let ds_ref = roto_pack.get_data_sources();

    for mb in roto_pack.get_mir().iter() {
        println!("{}", mb);
    }

    let peer_ip = "192.0.2.0".parse().unwrap();

    let provenance = Provenance {
        timestamp: chrono::Utc::now(),
        connection_id: "192.0.2.10:178".parse().unwrap(),
        peer_id: PeerId {
            addr: peer_ip,
            asn: Asn::from(65534),
        },
        peer_bgp_id: [0; 4].into(),
        peer_distuingisher: [0; 8],
        peer_rib_type: PeerRibType::OutPost,
    };

    let context =
        RouteContext::new(None, NlriStatus::InConvergence, provenance);

    let mut vm = vm::VmBuilder::new()
        // .with_arguments(args)
        .with_context(context)
        .with_data_sources(ds_ref)
        .with_mir_code(roto_pack.get_mir())
        .build()?;

    let mem = &mut vm::LinearMemory::uninit();
    let res = vm
        .exec(
            payload,
            None::<Record>,
            // Some(filter_map_arguments),
            None,
            mem,
        )
        .unwrap();

    trace!("\nRESULT");
    trace!("action: {}", res.accept_reject);
    trace!("rx    : {:?}", res.rx);
    trace!("tx    : {:?}", res.tx);

    Ok(res)
}

fn test_data_4(
    name: Scope,
    payload: TypeValue,
    source_code: &'static str,
) -> Result<VmResult, Box<dyn std::error::Error>> {
    println!("Evaluate filter-map {}...", name);

        // Compile the source code in this example
        let rotolo = Compiler::build(source_code)?;
        let roto_pack = rotolo.retrieve_pack_as_refs(&name)?;
    
        trace!("Used Arguments");
        trace!("{:#?}", &roto_pack.get_arguments());
        trace!("Used Data Sources");
        trace!("{:#?}", &roto_pack.get_data_sources());
    
        let ds_ref = roto_pack.get_data_sources();
    
        for mb in roto_pack.get_mir().iter() {
            println!("{}", mb);
        }
 
        let peer_ip = "192.0.2.0".parse().unwrap();

        let provenance = Provenance {
            timestamp: chrono::Utc::now(),
            connection_id: "192.0.2.10:178".parse().unwrap(),
            peer_id: PeerId {
                addr: peer_ip,
                asn: Asn::from(65534),
            },
            peer_bgp_id: [0; 4].into(),
            peer_distuingisher: [0; 8],
            peer_rib_type: PeerRibType::OutPost,
        };

        let context =
            RouteContext::new(None, NlriStatus::InConvergence, provenance);

        let mut vm = vm::VmBuilder::new()
            // .with_arguments(args)
            .with_context(context)
            .with_data_sources(ds_ref)
            .with_mir_code(roto_pack.get_mir())
            .build()?;
    
        let mem = &mut vm::LinearMemory::uninit();
        let res = vm
            .exec(
                payload,
                None::<Record>,
                // Some(filter_map_arguments),
                None,
                mem,
            )
            .unwrap();
    
        trace!("\nRESULT");
        trace!("action: {}", res.accept_reject);
        trace!("rx    : {:?}", res.rx);
        trace!("tx    : {:?}", res.tx);
    
        Ok(res)
}

fn initiation_payload_example() -> Vec<u8> {
    vec![
        0x03, 0x00, 0x00, 0x00, 0x34, 0x04, 0x00, 0x02, 0x00, 0x0d, 0x6d,
        0x79, 0x2d, 0x62, 0x6d, 0x70, 0x2d, 0x72, 0x6f, 0x75, 0x74, 0x65,
        0x72, 0x00, 0x01, 0x00, 0x19, 0x4d, 0x6f, 0x63, 0x6b, 0x20, 0x42,
        0x4d, 0x50, 0x20, 0x6d, 0x6f, 0x6e, 0x69, 0x74, 0x6f, 0x72, 0x65,
        0x64, 0x20, 0x72, 0x6f, 0x75, 0x74, 0x65, 0x72,
    ]
}

fn compile_initiation_payload(
    name: Scope,
    source_code: &'static str,
    buf: routecore::bmp::message::Message<bytes::Bytes>,
) -> Result<VmResult, Box<dyn std::error::Error>> {
    println!("Evaluate filter-map {}...", name);

    // Compile the source code in this example
    let rotolo = Compiler::build(source_code)?;
    let roto_pack = rotolo.retrieve_pack_as_refs(&name)?;

    // assert!(i_msg.is_ok());

    // let rm_msg = i_msg.unwrap();
    let payload = TypeValue::Builtin(
        roto::types::builtin::BuiltinTypeValue::BmpMessage(buf.into()),
    );

    trace!("Used Arguments");
    trace!("{:#?}", &roto_pack.get_arguments());
    trace!("Used Data Sources");
    trace!("{:#?}", &roto_pack.get_data_sources());

    let ds_ref = roto_pack.get_data_sources();

    for mb in roto_pack.get_mir().iter() {
        println!("{}", mb);
    }

    let peer_ip = "192.0.2.0".parse().unwrap();

    let provenance = Provenance {
        timestamp: chrono::Utc::now(),
        connection_id: "192.0.2.10:178".parse().unwrap(),
        peer_id: PeerId {
            addr: peer_ip,
            asn: Asn::from(65534),
        },
        peer_bgp_id: [0; 4].into(),
        peer_distuingisher: [0; 8],
        peer_rib_type: PeerRibType::OutPost,
    };

    let context =
        RouteContext::new(None, NlriStatus::InConvergence, provenance);

    let mut vm = vm::VmBuilder::new()
        // .with_arguments(args)
        .with_context(context)
        .with_data_sources(ds_ref)
        .with_mir_code(roto_pack.get_mir())
        .build()?;

    let mem = &mut vm::LinearMemory::uninit();
    let res = vm
        .exec(
            payload,
            None::<Record>,
            // Some(filter_map_arguments),
            None,
            mem,
        )
        .unwrap();

    trace!("\nRESULT");
    trace!("action: {}", res.accept_reject);
    trace!("rx    : {:?}", res.rx);
    trace!("tx    : {:?}", res.tx);

    Ok(res)
}

fn route_monitoring_example() -> Vec<u8> {
    vec![
        0x03, 0x00, 0x00, 0x00, 0x67, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0a, 0xff, 0x00, 0x65, 0x00,
        0x01, 0x00, 0x00, 0x0a, 0x0a, 0x0a, 0x01, 0x54, 0xa2, 0x0e, 0x0c,
        0x00, 0x0e, 0x81, 0x09, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x37,
        0x02, 0x00, 0x00, 0x00, 0x1b, 0x40, 0x01, 0x01, 0x00, 0x40, 0x02,
        0x06, 0x02, 0x01, 0x00, 0x01, 0x00, 0x00, 0x40, 0x03, 0x04, 0x0a,
        0xff, 0x00, 0x65, 0x80, 0x04, 0x04, 0x00, 0x00, 0x00, 0x01, 0x20,
        0x0a, 0x0a, 0x0a, 0x02,
    ]
}

#[test]
fn initiation_msg_test_1() {
    common::init();

    let payl = initiation_payload_example();

    let i_msg = routecore::bmp::message::Message::from_octets(
        bytes::Bytes::from(payl),
    )
    .unwrap();

    assert!(matches!(
        i_msg,
        routecore::bmp::message::Message::InitiationMessage(_)
    ));

    let res = compile_initiation_payload(
        Filter("bmp-in-filter".into()),
        r#"
        filter bmp-in-filter {
            define {
                rx msg: BmpMessage;
            }

            term is_rm_ipv4 with msg: BmpRouteMonitoringMessage {
                match {
                    msg.per_peer_header.is_ipv4;
                }
            }

            term is_pd_ipv4 with msg: BmpPeerDownNotification {
                match {
                    msg.per_peer_header.is_ipv4;
                }
            }

            apply {
                match msg with {
                    RouteMonitoring(rm_msg) | is_rm_ipv4(rm_msg) -> { return accept; },
                    PeerDownNotification(pd_msg) -> { return accept; },
                    InitiationMessage(i_msg) -> { return accept; },
                }
                reject;
            }  
        }
        "#,
        i_msg
    ).unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn initiation_msg_test_2() {
    common::init();

    let payl = initiation_payload_example();

    let i_msg = routecore::bmp::message::Message::from_octets(
        bytes::Bytes::from(payl),
    )
    .unwrap();

    assert!(matches!(
        i_msg,
        routecore::bmp::message::Message::InitiationMessage(_)
    ));

    let res = compile_initiation_payload(
        Filter("bmp-in-filter".into()),
        r#"filter bmp-in-filter {
        define {
            rx msg: BmpMessage;
        }

        term ipv6_only {
            match msg with {
                RouteMonitoring(rm_msg) -> 
                    rm_msg.per_peer_header.is_ipv6,
                PeerDownNotification(pd_msg) -> { 
                    pd_msg.per_peer_header.is_ipv6; 
                },
                InitiationMessage(i_msg) -> {
                    1 == 1;
                },
                PeerUpNotification(pu_msg) -> {
                    pu_msg.per_peer_header.is_ipv6;
                },
            }
        }

        apply {
            filter match ipv6_only matching {
                return accept;
            };
            reject;
        }   
    }"#,
        i_msg,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn initiation_msg_test_3() {
    common::init();

    let payl = initiation_payload_example();

    let i_msg = routecore::bmp::message::Message::from_octets(
        bytes::Bytes::from(payl),
    )
    .unwrap();

    assert!(matches!(
        i_msg,
        routecore::bmp::message::Message::InitiationMessage(_)
    ));

    let res = compile_initiation_payload(
        Filter("bmp-in-filter".into()),
        r#"filter bmp-in-filter {
        define {
            rx msg: BmpMessage;
        }
    
        term ipv6_only {
            match msg with {
                RouteMonitoring(rm_msg) -> rm_msg.per_peer_header.is_ipv6,
                PeerDownNotification(pd_msg) -> { pd_msg.per_peer_header.is_ipv6; },
                PeerUpNotification(pu_msg) -> {
                    1 != 1;
                },
                InitiationMessage(i_msg) -> {
                    1 == 1;
                }
            }
        }
    
        apply {
            filter match ipv6_only matching {
                return accept;
            };
            reject;
        }  
    }"#,
        i_msg,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn bmp_route_monitoring_1() {
    common::init();

    let payl = route_monitoring_example();

    let pd_msg = routecore::bmp::message::Message::from_octets(
        bytes::Bytes::from(payl),
    )
    .unwrap();

    trace!("{}", pd_msg);
    assert!(matches!(
        pd_msg,
        routecore::bmp::message::Message::RouteMonitoring(_)
    ));

    let res = compile_initiation_payload(
        Filter("bmp-in-filter".into()),
        r#"filter bmp-in-filter {
        define {
            rx msg: BmpMessage;
        }

        term ipv6_only {
            match msg with {
                RouteMonitoring(rm_msg) -> 
                    rm_msg.per_peer_header.is_ipv6,
                PeerDownNotification(pd_msg) -> { 
                    pd_msg.per_peer_header.is_ipv6; 
                },
                InitiationMessage(i_msg) -> {
                    1 != 1;
                    1 == 1;
                },
                PeerUpNotification(pu_msg) -> {
                    pu_msg.per_peer_header.is_ipv6;
                },
            }
        }

        apply {
            filter match ipv6_only matching {
                return accept;
            };
            reject;
        }   
    }"#,
        pd_msg,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn bmp_message_1() {
    common::init();

    let res = test_data(
        FilterMap("filter-v4-only".into()),
        r###"
        filter-map filter-v4-only {
            define {
                rx_tx rm_msg: BmpRouteMonitoringMessage;
            }

            term is_ipv4_msg {
                match {
                    rm_msg.per_peer_header.is_ipv4 == true;
                }
            }
        
            apply {
                filter match is_ipv4_msg matching {
                    return accept;
                };
                reject;
            }
        }
        "###,
        route_monitoring_example(),
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn bmp_message_2() {
    common::init();

    let res = test_data(
        FilterMap("filter-v6-only".into()),
        r###"
        filter-map filter-v6-only {
            define {
                rx_tx rm_msg: BmpRouteMonitoringMessage;
            }

            term is_ipv6_msg {
                match {
                    rm_msg.per_peer_header.is_ipv6 == true;
                }
            }
        
            apply {
                filter match is_ipv6_msg matching {
                    return accept;
                };
                reject;
            }
        }
        "###,
        route_monitoring_example(),
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn bmp_message_3() {
    common::init();

    let res = test_data(
        FilterMap("filter-v6-only".into()),
        r###"
        filter-map filter-v6-only {
            define {
                rx_tx rm_msg: BmpRouteMonitoringMessage;
            }

            term is_ipv6_msg {
                match {
                    rm_msg.per_peer_header;
                }
            }
        
            apply {
                filter match is_ipv6_msg matching {
                    return accept;
                };
                reject;
            }
        }
        "###,
        route_monitoring_example(),
    );

    let err =
        "Eval error: Cannot convert value with type Lazy Record".to_string();
    let mut str = res.unwrap_err().to_string();
    str.truncate(err.len());
    assert_eq!(str, err);
}

#[test]
fn bmp_message_4() {
    common::init();

    let res = test_data_2(
        Filter("filter-v6-only".into()),
        r###"
        filter filter-v6-only {
            define {
                rx msg: BmpMessage;
            }

            term rm_only {
                match msg with {
                    RouteMonitoring(rm_msg) -> rm_msg.per_peer_header.is_ipv4,
                    PeerDownNotification(pd_msg) -> { pd_msg.per_peer_header.is_ipv4; },
                    PeerUpNotification(pu_msg) -> {
                        pu_msg.per_peer_header.is_ipv4;
                        pu_msg.per_peer_header.peer_type == 1;
                    },
                }
            }
        
            apply {
                filter match rm_only matching {
                    return accept;
                };
                reject;
            }
        }
        "###,
    );

    trace!("res : {:?}", res);

    res.unwrap();
    // let err =
    //     "Eval error: Cannot convert value with type Lazy Record".to_string();
    // let mut str = res.unwrap_err().to_string();
    // str.truncate(err.len());
    // assert_eq!(str, err);
}

#[test]
fn bmp_message_5() {
    common::init();

    let res = test_data_3(
        Filter("filter-v6-only".into()),
        r###"
        filter filter-v6-only {
            define {
                rx msg: BmpMessage;
            }

            term rm_only {
                match msg with {
                    RouteMonitoring(rm_msg) -> rm_msg.per_peer_header.is_ipv4,
                    PeerDownNotification(pd_msg) -> { pd_msg.per_peer_header.peer_type == 23; },
                    PeerUpNotification(pu_msg) -> {
                        pu_msg.per_peer_header.is_ipv4;
                    },
                }
            }
        
            apply {
                filter match rm_only matching {
                    return accept;
                };
                reject;
            }
        }
        "###,
    );

    trace!("res : {:?}", res);

    res.unwrap();
    // let err =
    //     "Eval error: Cannot convert value with type Lazy Record".to_string();
    // let mut str = res.unwrap_err().to_string();
    // str.truncate(err.len());
    // assert_eq!(str, err);
}

#[test]
fn bmp_message_6() {
    common::init();

    let res = test_data_3(
        Filter("is-rm-ipv4".into()),
        r#"
        filter is-rm-ipv4 {
            define {
                rx msg: BmpMessage;
            }

            term is_rm_ipv4 with xx_msg: BmpRouteMonitoringMessage {
                match {
                    xx_msg.per_peer_header.is_ipv4;
                }
            }

            action send_msg with yy_msg: BmpPeerDownNotification {
                mqtt.send(
                    {
                        asn: yy_msg.per_peer_header.asn,
                        message: String.format(
                            "Peer with ASN {} just went down.", 
                            yy_msg.per_peer_header.peer_type
                        )
                    }
                );
            }
        
            apply {
                match msg with {
                    RouteMonitoring(rm_msg) | is_rm_ipv4(rm_msg) -> { return accept; },
                    PeerDownNotification(pd_msg) -> { send_msg(pd_msg); },
                }
                reject;
            }
        }

        output-stream mqtt contains Message {
            asn: Asn,
            message: String
        }
        "#,
    );

    trace!("res : {:?}", res);

    res.unwrap();
    // let err =
    //     "Eval error: Cannot convert value with type Lazy Record".to_string();
    // let mut str = res.unwrap_err().to_string();
    // str.truncate(err.len());
    // assert_eq!(str, err);
}

#[test]
fn bmp_message_7() {
    common::init();

    let res = test_data_3(
        Filter("is-rm-ipv4".into()),
        r#"
        filter is-rm-ipv4 {
            define {
                rx msg: BmpMessage;
            }

            term is_rm_ipv4 with xx_msg: BmpPeerDownNotification {
                match {
                    xx_msg.per_peer_header.is_legacy_format;
                }
            }

            action send_msg with yy_msg: BmpPeerDownNotification {
                mqtt.send(
                    {
                        asn: yy_msg.per_peer_header.asn,
                        message: String.format(
                            "Peer with ASN {} just went down.", 
                            yy_msg.per_peer_header.is_legacy_format
                        )
                    }
                );
            }
        
            apply {
                match msg with {
                    PeerDownNotification(rm_msg) | is_rm_ipv4(rm_msg) -> { send_msg(rm_msg); },
                    RouteMonitoring(pd_msg) -> { return accept; },
                }
                reject;
            }
        }

        output-stream mqtt contains Message {
            asn: Asn,
            message: String
        }
        "#,
    );

    trace!("res : {:?}", res);

    res.unwrap();
    // let err =
    //     "Eval error: Cannot convert value with type Lazy Record".to_string();
    // let mut str = res.unwrap_err().to_string();
    // str.truncate(err.len());
    // assert_eq!(str, err);
}

#[test]
fn bmp_message_8() {
    common::init();

    let res = test_data_3(
        Filter("is-rm-ipv4".into()),
        r#"
        filter is-rm-ipv4 {
            define {
                rx msg: BmpMessage;
                bla = "my_string";
            }

            term is_legacy_format with xx_msg: BmpRouteMonitoringMessage {
                match {
                    xx_msg.per_peer_header.is_legacy_format;
                }
            }

            term is_rm_ipv6 with zz_msg: BmpRouteMonitoringMessage {
                match {
                    zz_msg.per_peer_header.is_ipv6;
                }
            }

            action send_msg with yy_msg: IntegerLiteral {
                mqtt.send(
                    {
                        asn: yy_msg,
                        message: yy_msg
                    }
                );
            }
        
            apply {
                match msg with {
                    PeerDownNotification(rm_msg) | is_rm_ipv6(bla) -> { return accept; },
                    RouteMonitoring(pd_msg) | is_legacy_format(pd_msg) -> { send_msg(pd_msg); },
                }
                reject;
            }
        }

        output-stream mqtt contains Message {
            asn: Asn,
            message: IntegerLiteral
        }
        "#,
    );

    trace!("res : {:?}", res);

    // res.unwrap();
    let err =
        "Eval error: String cannot be converted into Lazy Record".to_string();
    let mut str = res.unwrap_err().to_string();
    str.truncate(err.len());
    assert_eq!(str, err);
}

const TEST_ROUTER_SYS_NAME: &str = "test-router";
const TEST_ROUTER_SYS_DESC: &str = "test-desc";

fn mk_initiation_msg() -> bytes::Bytes {
    routes::bmp::encode::mk_initiation_msg(
        TEST_ROUTER_SYS_NAME,
        TEST_ROUTER_SYS_DESC,
    )
}

#[test]
fn bmp_message_9() {
    common::init();

    let buf = mk_initiation_msg();
    let rm_msg = BytesRecord::<InitiationMessage>::new(buf);
    assert!(rm_msg.is_ok());
    let rm_msg = rm_msg.unwrap();
    let payload = TypeValue::Builtin(
        roto::types::builtin::BuiltinTypeValue::BmpInitiationMessage(rm_msg),
    );

    let res = test_data_4(
        Filter("my-module".into()),
        payload,
        r#"
        filter my-module {
            define {
                rx msg: BmpMessage;
            }

            term has_asn {
                // Compare the ASN for BMP message types that have a Per Peer Header
                // We can't omit the other message types as without the explicit
                // 1 == 1 (true) check the resulting logic isn't what we want.
                match msg with {
                    InitiationMessage(i_msg) -> 1 == 1,
                    PeerDownNotification(pd_msg) -> pd_msg.per_peer_header.asn == AS12345,
                    PeerUpNotification(pu_msg) -> pu_msg.per_peer_header.asn == AS12345,
                    RouteMonitoring(rm_msg) -> rm_msg.per_peer_header.asn == AS12345,
                    StatisticsReport(sr_msg) -> sr_msg.per_peer_header.asn == AS12345,
                    TerminationMessage(t_msg) -> 1 == 1,
                }
            }

            apply {
                filter match has_asn matching {
                    return accept;
                };
                reject;
            }
        }"#
    ).unwrap();

    trace!("res : {:?}", res);
    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn bmp_message_10() {
    common::init();

    let buf = vec![
        0x03, 0x00, 0x00, 0x00, 0x47, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x7f, 0x00, 0x00, 0x01, 0x00,
        0x00, 0x30, 0x39, 0x01, 0x02, 0x03, 0x04, 0x65, 0x7c, 0x49, 0xa7,
        0x00, 0x0a, 0x13, 0x82, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x17,
        0x02, 0x00, 0x00, 0x00, 0x00,
    ];
    let rm_msg = BytesRecord::<BmpMessage>::new(buf.into());
    assert!(rm_msg.is_ok());
    let rm_msg = rm_msg.unwrap();
    let payload = TypeValue::Builtin(
        roto::types::builtin::BuiltinTypeValue::BmpMessage(rm_msg),
    );

    let res = test_data_4(
        Filter("my-module".into()),
        payload.clone(),
        r#"
        filter my-module {
            define {
                rx msg: BmpMessage;
            }

            term has_asn {
                // Compare the ASN for BMP message types that have a Per Peer Header
                // We can't omit the other message types as without the explicit
                // 1 == 1 (true) check the resulting logic isn't what we want.
                match msg with {
                    InitiationMessage(i_msg) -> 1 == 1,
                    PeerDownNotification(pd_msg) -> pd_msg.per_peer_header.asn == AS12345,
                    PeerUpNotification(pu_msg) -> pu_msg.per_peer_header.asn == AS12345,
                    RouteMonitoring(rm_msg) -> rm_msg.per_peer_header.asn == AS12345,
                    StatisticsReport(sr_msg) -> sr_msg.per_peer_header.asn == AS12345,
                    TerminationMessage(t_msg) -> 1 == 1,
                }
            }

            apply {
                filter match has_asn matching {
                    return accept;
                };
                reject;
            }
        }"#
    ).unwrap();

    trace!("res : {:?}", res);

    assert_eq!(res.accept_reject, AcceptReject::Accept);
    assert_eq!(res.rx, payload);
}

#[test]
fn initiation_message() {
    common::init();

    let buf = mk_initiation_msg();
    let im_msg = BytesRecord::<BmpMessage>::new(buf);
    assert!(im_msg.is_ok());
    let im_msg = im_msg.unwrap();
    let payload = TypeValue::Builtin(
        roto::types::builtin::BuiltinTypeValue::BmpMessage(im_msg),
    );

    let res = test_data_4(
        Filter("my-module".into()),
        payload,
        r#"
        filter my-module {
            define {
                rx msg: BmpMessage;
            }

            term has_asn {
                // Compare the ASN for BMP message types that have a Per Peer Header
                // We can't omit the other message types as without the explicit
                // 1 == 1 (true) check the resulting logic isn't what we want.
                match msg with {
                    PeerDownNotification(pd_msg) -> pd_msg.per_peer_header.asn == AS12345,
                    PeerUpNotification(pu_msg) -> pu_msg.per_peer_header.asn == AS12345,
                    InitiationMessage(i_msg) -> 1 == 1,
                    RouteMonitoring(rm_msg) -> rm_msg.per_peer_header.asn == AS12345,
                    StatisticsReport(sr_msg) -> sr_msg.per_peer_header.asn == AS12345,
                    TerminationMessage(t_msg) -> 1 == 1,
                }
            }

            apply {
                filter match has_asn matching {
                    return accept;
                };
                reject;
            }
        }"#
    ).unwrap();

    trace!("res : {:?}", res);
    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn peer_down_notification_1() {
    let pph = mk_per_peer_header("192.0.2.10", 65536);
    let peer_up: BytesRecord<BmpMessage> =
        BytesRecord::<BmpMessage>::new(mk_peer_down_notification_msg(&pph).0)
            .unwrap();
    let payload: TypeValue = TypeValue::Builtin(peer_up.into());
    println!("payload {:?}", payload);

    let res = test_data_4(
        Filter("my-module".into()),
        payload,
        r#"
        filter my-module {
            define {
                rx msg: BmpMessage;
            }

            term has_asn {
                // Compare the ASN for BMP message types that have a Per Peer Header
                // We can't omit the other message types as without the explicit
                // 1 == 1 (true) check the resulting logic isn't what we want.
                match msg with {
                    PeerDownNotification(pd_msg) -> pd_msg.per_peer_header.asn == AS65536,
                    PeerUpNotification(pu_msg) -> pu_msg.per_peer_header.asn == AS12345,
                    InitiationMessage(i_msg) -> 1 == 1,
                    RouteMonitoring(rm_msg) -> rm_msg.per_peer_header.asn == AS12345,
                    StatisticsReport(sr_msg) -> sr_msg.per_peer_header.asn == AS12345,
                    TerminationMessage(t_msg) -> 1 == 1,
                }
            }

            apply {
                filter match has_asn matching {
                    return accept;
                };
                reject;
            }
        }"#
    ).unwrap();

    trace!("res : {:?}", res);
    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn peer_down_notification_2() {
    let pph = mk_per_peer_header("192.0.2.10", 65536);
    let peer_up: BytesRecord<BmpMessage> =
        BytesRecord::<BmpMessage>::new(mk_peer_down_notification_msg(&pph).0)
            .unwrap();
    let payload: TypeValue = TypeValue::Builtin(peer_up.into());
    println!("payload {:?}", payload);

    let res = test_data_4(
        Filter("my-module".into()),
        payload,
        r#"
        filter my-module {
            define {
                rx msg: BmpMessage;
            }

            term has_asn {
                // Compare the ASN for BMP message types that have a Per Peer Header
                // We can't omit the other message types as without the explicit
                // 1 == 1 (true) check the resulting logic isn't what we want.
                match msg with {
                    PeerDownNotification(pd_msg) -> pd_msg.per_peer_header.asn == AS12345,
                    PeerUpNotification(pu_msg) -> pu_msg.per_peer_header.asn == AS12345,
                    InitiationMessage(i_msg) -> 1 == 1,
                    RouteMonitoring(rm_msg) -> rm_msg.per_peer_header.asn == AS12345,
                    StatisticsReport(sr_msg) -> sr_msg.per_peer_header.asn == AS12345,
                    TerminationMessage(t_msg) -> 1 == 1,
                }
            }

            apply {
                filter match has_asn matching {
                    return accept;
                };
                reject;
            }
        }"#
    ).unwrap();

    trace!("res : {:?}", res);
    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn termination_message_1() {
    let peer_up: BytesRecord<BmpMessage> =
        BytesRecord::<BmpMessage>::new(mk_termination_msg()).unwrap();
    let btv: BuiltinTypeValue = peer_up.into();
    let expected: Result<TypeValue, CompileError> = Err(CompileError::from(
        "Cannot convert raw BMP message into any other type.",
    ));
    assert_eq!(
        btv.clone().into_type(&TypeDef::LazyRecord(
            LazyRecordTypeDef::UpdateMessage
        )),
        expected
    );
    let payload: TypeValue = TypeValue::Builtin(btv);
    println!("payload {:?}", payload);

    let res = test_data_4(
        Filter("my-module".into()),
        payload,
        r#"
        filter my-module {
            define {
                rx msg: BmpMessage;
            }

            term has_asn {
                // Compare the ASN for BMP message types that have a Per Peer Header
                // We can't omit the other message types as without the explicit
                // 1 == 1 (true) check the resulting logic isn't what we want.
                match msg with {
                    PeerDownNotification(pd_msg) -> pd_msg.per_peer_header.asn == AS12345,
                    PeerUpNotification(pu_msg) -> pu_msg.per_peer_header.asn == AS12345,
                    InitiationMessage(i_msg) -> 1 == 1,
                    RouteMonitoring(rm_msg) -> rm_msg.per_peer_header.asn == AS12345,
                    StatisticsReport(sr_msg) -> sr_msg.per_peer_header.asn == AS12345,
                    TerminationMessage(t_msg) -> 1 == 1,
                }
            }

            apply {
                filter match has_asn matching {
                    return accept;
                };
                reject;
            }
        }"#
    ).unwrap();

    trace!("res : {:?}", res);
    assert_eq!(res.accept_reject, AcceptReject::Accept);
}
