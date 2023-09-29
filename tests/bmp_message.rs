use std::sync::Arc;

use log::trace;
use roto::{
    ast::AcceptReject,
    compile::Compiler,
    blocks::Scope,
    blocks::Scope::{Filter, FilterMap},
    types::{
        builtin::BytesRecord, collections::Record,
        lazyrecord_types::{RouteMonitoring, BmpMessage}, typevalue::TypeValue,
    },
    vm::{self, VmResult},
};

mod common;

fn test_data(
    name: Scope,
    source_code: &'static str,
) -> Result<VmResult, Box<dyn std::error::Error>> {
    println!("Evaluate filter-map {}...", name);

    // Compile the source code in this example
    let rotolo = Compiler::build(source_code)?;
    let roto_pack = rotolo.retrieve_pack_as_arcs(&name)?;

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

    let rm_msg = BytesRecord::<RouteMonitoring>::new(buf.clone().into());
    assert!(rm_msg.is_ok());
    let rm_msg = rm_msg.unwrap();
    let payload = TypeValue::Builtin(
        roto::types::builtin::BuiltinTypeValue::BmpRouteMonitoringMessage(
            Arc::new(rm_msg),
        ),
    );

    trace!("BUF {}", routecore::bmp::message::Message::from_octets(buf.clone()).unwrap());
    trace!("Used Arguments");
    trace!("{:#?}", &roto_pack.get_arguments());
    trace!("Used Data Sources");
    trace!("{:#?}", &roto_pack.get_data_sources());

    let ds_ref = roto_pack.get_data_sources();

    for mb in roto_pack.get_mir().iter() {
        println!("{}", mb);
    }

    let mut vm = vm::VmBuilder::new()
        // .with_arguments(args)
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
    let roto_pack = rotolo.retrieve_pack_as_arcs(&name)?;

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
        roto::types::builtin::BuiltinTypeValue::BmpMessage(
            Arc::new(rm_msg),
        ),
    );

    trace!("Used Arguments");
    trace!("{:#?}", &roto_pack.get_arguments());
    trace!("Used Data Sources");
    trace!("{:#?}", &roto_pack.get_data_sources());

    let ds_ref = roto_pack.get_data_sources();

    for mb in roto_pack.get_mir().iter() {
        println!("{}", mb);
    }

    let mut vm = vm::VmBuilder::new()
        // .with_arguments(args)
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
) -> Result<VmResult, Box<dyn std::error::Error>>  {
    println!("Evaluate filter-map {}...", name);

    // Compile the source code in this example
    let rotolo = Compiler::build(source_code)?;
    let roto_pack = rotolo.retrieve_pack_as_arcs(&name)?;

    // BMP PeerDownNotification type 3, containing a BGP NOTIFICATION.
    let buf = vec![
        0x03, 0x00, 0x00, 0x00, 0x46, 0x02, 0x00, 0x80,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x20, 0x01, 0x0d, 0xb8, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01,
        0x00, 0x01, 0x00, 0x00, 0x0a, 0x00, 0x00, 0x0a,
        0x62, 0x2d, 0xea, 0x80, 0x00, 0x05, 0x58, 0x22,
        0x03, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0x00, 0x15, 0x03, 0x06, 0x02
    ];

    // let bla = BmpMessage::from_octets(buf.into()).unwrap();
    let rm_msg = BytesRecord::<BmpMessage>::new(buf.into());
    assert!(rm_msg.is_ok());
    let rm_msg = rm_msg.unwrap();
    let payload = TypeValue::Builtin(
        roto::types::builtin::BuiltinTypeValue::BmpMessage(
            Arc::new(rm_msg),
        ),
    );

    trace!("Used Arguments");
    trace!("{:#?}", &roto_pack.get_arguments());
    trace!("Used Data Sources");
    trace!("{:#?}", &roto_pack.get_data_sources());

    let ds_ref = roto_pack.get_data_sources();

    for mb in roto_pack.get_mir().iter() {
        println!("{}", mb);
    }

    let mut vm = vm::VmBuilder::new()
        // .with_arguments(args)
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
                            yy_msg.per_peer_header.is_legacy_format
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