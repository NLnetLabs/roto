use std::collections::BTreeSet;
use std::str::FromStr;

use log::trace;
use roto::ast::AcceptReject;

use roto::blocks::Scope::{self, Filter};
use roto::pipeline;
use roto::types::builtin::basic_route::{
    BasicRouteToken, PeerId, PeerRibType, Provenance,
};
use roto::types::builtin::{explode_announcements, BuiltinTypeValue, NlriStatus, RouteContext};
use roto::types::collections::{BytesRecord, Record};
use roto::types::lazyrecord_types::BgpUpdateMessage;
use roto::types::typevalue::TypeValue;
use roto::vm::{self, FieldIndex, VmResult};
use inetnum::addr::Prefix;
use inetnum::asn::Asn;
use routecore::bgp::message::update_builder::UpdateBuilder;
use routecore::bgp::nlri::afisafi::{Ipv6UnicastNlri, Nlri};
use routecore::bgp::nlri::afisafi::IsPrefix;
use routecore::bgp::message::SessionConfig;
use routecore::bgp::types::{LocalPref, NextHop};
use routecore::bgp::workshop::route::RouteWorkshop;

mod common;

fn test_data(
    name: Scope,
    source_code: &str,
) -> Result<(VmResult, TypeValue, TypeValue), Box<dyn std::error::Error>> {
    trace!("Evaluate filter-map {}...", name);

    let rotolo = pipeline::run_test(source_code, None)?;
    let roto_pack = rotolo.retrieve_pack_as_refs(&name)?;

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

    let update = BytesRecord::<BgpUpdateMessage>::new(
        buf.clone(),
        SessionConfig::modern(),
    )
    .unwrap();

    let first_prefix: Prefix = update
        .bytes_parser()
        .announcements()
        .into_iter()
        .next()
        .and_then(|mut nlri| {
            if let Some(Ok(Nlri::Ipv6Unicast(nlri))) =
                nlri.next()
            {
                Some(nlri.prefix())
            } else {
                None
            }
        })
        .unwrap();
    let peer_ip = "fe80::1".parse().unwrap();

    let provenance = Provenance {
        timestamp: chrono::Utc::now(),
        connection_id: "[fe80::1]:178".parse().unwrap(),
        peer_id: PeerId {
            addr: peer_ip,
            asn: Asn::from(65534),
        },
        peer_bgp_id: [0; 4].into(),
        peer_distuingisher: [0; 8],
        peer_rib_type: PeerRibType::OutPost,
    };

    // let nlri: Ipv6UnicastNlri = update
    //     .bytes_parser()
    //     .announcements_vec()
    //     .unwrap()
    //     .first()
    //     .unwrap()
    //     .clone()
    //     .try_into()
    //     .unwrap();

    let context = &RouteContext::new(
        Some(update.clone()),
        NlriStatus::InConvergence,
        provenance,
    );

    // let update: UpdateMessage<bytes::Bytes> = update.into_inner();
    // let parser = Parser::from_ref(update.octets());

    // let pa_map = PaMap::from_update_pdu(&update).unwrap();
    let parser = update.bytes_parser();
    let afi_safis = parser.announcement_fams();
    trace!("afi safis {:?}", afi_safis.collect::<Vec<_>>());

    #[allow(clippy::mutable_key_type)]
    let mut nlri_set = BTreeSet::new();
    
    let rws = explode_announcements(parser, &mut nlri_set);
    
    trace!("rws {:#?}", rws);
    // let mut rws = RouteWorkshop::from_update_pdu(nlri, &update)?;

    // from_update_pdu does NOT set MP_REACH_NLRI attribute, so we have to set
    // the NLRI and the NextHop manually.

    // Get the NLRI for this route, we're only looking at the first
    // announcement, so get that one.
    // let announces = parser.announcements_vec().unwrap();

    // Create a MpReachBuilder with the intended NLRI
    // let mp_reach =
    //     MpReachNlriBuilder::for_nlri(announces.first().unwrap());
    let rws = &mut rws.unwrap();

    let payload = &mut rws.get_mut(0).unwrap();

    trace!("peer_ip {:?}", context.provenance().peer_ip());

    let mem = &mut vm::LinearMemory::uninit();

    println!("Used Arguments");
    println!("{:#?}", &roto_pack.arguments);
    println!("Used Data Sources");
    println!("{:#?}", &roto_pack.data_sources);

    for mb in roto_pack.get_mir().iter() {
        println!("{}", mb);
    }

    let mut vm = vm::VmBuilder::new()
        .with_data_sources(roto_pack.data_sources)
        .with_mir_code(roto_pack.mir)
        .with_context(context)
        .build()?;

    let res = vm.exec(payload.clone(), None::<Record>, None, mem).unwrap();

    println!("\nRESULT");
    println!("action: {}", res.accept_reject);
    println!("rx    : {:?}", res.rx);
    println!("tx    : {:?}", res.tx);

    Ok((res, first_prefix.into(), peer_ip.into()))
}

#[test]
fn test_routes_1() {
    common::init();
    let src = r#"
        filter rib-in-pre-filter {
            define {
                rx route: Route;
                context ctx: RouteContext;
            }
        
            term always {
                match {
                    1 == 1;
                }
            }
        
            action send_msg {
                // An untyped anonymous record
                mqtt.send(
                    {
                        prefix: route.prefix,
                        peer_ip: ctx.provenance.peer-id.addr
                    }
                );
            }
        
            apply {
                filter match always matching {
                    send_msg;
                    return accept;
                };
                reject;
            }
        }
        
        output-stream mqtt contains Message {
            prefix: Prefix,
            peer_ip: IpAddress
        }
    "#;

    let test_run = test_data(Filter("rib-in-pre-filter".into()), src);

    let (
        VmResult {
            accept_reject,
            output_stream_queue,
            ..
        },
        prefix,
        peer_ip,
    ) = test_run.unwrap();

    trace!("OUTPUT RECORD");
    let output_record = output_stream_queue[0].get_record();
    trace!("{:#?}", output_record);
    assert_eq!(output_stream_queue.len(), 1);

    // The outputted record is an ordered_record, meaning it is sorted
    // alphabetically on the key, so [0] is "peer_ip" and [1] is "prefix"
    assert_eq!(
        output_record.get_field_by_index(vec![1].into()).unwrap(),
        &prefix
    );
    assert_eq!(
        output_record.get_field_by_index(vec![0].into()).unwrap(),
        &peer_ip
    );
    trace!("{:#?}", output_stream_queue);
    assert_eq!(accept_reject, AcceptReject::Accept);
}

#[test]
fn test_routes_2() {
    common::init();
    let src = r#"
        filter rib-in-pre-filter {
            define {
                rx route: Route;
                context ctx: RouteContext;
            }
        
            term always {
                match {
                    1 == 1;
                }
            }
        
            // A typed anonymous record
            action send_msg {
                mqtt.send(
                    {
                        prefix: route.prefix,
                        peer_ip: ctx.provenance.peer-id.addr
                    }
                );
            }
        
            apply {
                filter match always matching {
                    send_msg;
                    return accept;
                };
                reject;
            }
        }
        
        output-stream mqtt contains Message {
            prefix: Prefix,
            peer_ip: IpAddress
        }
    "#;

    let test_run = test_data(Filter("rib-in-pre-filter".into()), src);

    let (
        VmResult {
            accept_reject,
            output_stream_queue,
            ..
        },
        prefix,
        peer_ip,
    ) = test_run.unwrap();
    let output_record = output_stream_queue[0].get_record();
    trace!("{:#?}", output_record);
    assert_eq!(output_stream_queue.len(), 1);

    // The outputted record is an ordered_record, meaning it is sorted
    // alphabetically on the key, so [0] is "peer_ip" and [1] is "prefix"
    assert_eq!(
        output_record.get_field_by_index(vec![1].into()).unwrap(),
        &prefix
    );
    assert_eq!(
        output_record.get_field_by_index(vec![0].into()).unwrap(),
        &peer_ip
    );
    trace!("{:#?}", output_stream_queue);
    assert_eq!(accept_reject, AcceptReject::Accept);
}

#[test]
fn test_routes_3() {
    common::init();
    let src = r#"
        filter rib-in-pre-filter {
            define {
                rx route: Route;
                context ctx: RouteContext;

                msg = Message {
                    prefix: route.prefix,
                    peer_ip: ctx.provenance.peer-id.addr
                };
            }
        
            term always {
                match {
                    msg.prefix.exists();
                }
            }
        
            // A typed named record
            action send_msg {
                mqtt.send(msg);
            }
        
            apply {
                filter match always matching {
                    send_msg;
                    return accept;
                };
                reject;
            }
        }
        
        output-stream mqtt contains Message {
            prefix: Prefix,
            peer_ip: IpAddress
        }
    "#;

    let test_run = test_data(Filter("rib-in-pre-filter".into()), src);

    let (
        VmResult {
            accept_reject,
            output_stream_queue,
            ..
        },
        prefix,
        peer_ip,
    ) = test_run.unwrap();
    let output_record = output_stream_queue[0].get_record();
    trace!("{:#?}", output_record);
    assert_eq!(output_stream_queue.len(), 1);

    // The outputted record is an ordered_record, meaning it is sorted
    // alphabetically on the key, so [0] is "peer_ip" and [1] is "prefix"
    assert_eq!(
        output_record.get_field_by_index(vec![1].into()).unwrap(),
        &prefix
    );
    assert_eq!(
        output_record.get_field_by_index(vec![0].into()).unwrap(),
        &peer_ip
    );
    trace!("{:#?}", output_stream_queue);
    assert_eq!(accept_reject, AcceptReject::Accept);
}

#[test]
fn test_routes_4() {
    common::init();
    let src = r#"
        filter rib-in-pre-filter {
            define {
                rx route: Route;
                context ctx: RouteContext;

                pfx = route.prefix;
                peer = ctx.provenance.peer-id.addr;
            }
        
            term always {
                match {
                    1 == 1;
                }
            }
        
            // A typed named record
            action send_msg {
                mqtt.send({
                    prefix: pfx,
                    peer_ip: peer
                });
            }
        
            apply {
                filter match always matching {
                    send_msg;
                    return accept;
                };
                reject;
            }
        }
        
        output-stream mqtt contains Message {
            prefix: Prefix,
            peer_ip: IpAddress
        }
    "#;

    let test_run = test_data(Filter("rib-in-pre-filter".into()), src);

    let (
        VmResult {
            accept_reject,
            output_stream_queue,
            ..
        },
        prefix,
        peer_ip,
    ) = test_run.unwrap();
    let output_record = output_stream_queue[0].get_record();
    trace!("{:#?}", output_record);
    assert_eq!(output_stream_queue.len(), 1);

    // The outputted record is an ordered_record, meaning it is sorted
    // alphabetically on the key, so [0] is "peer_ip" and [1] is "prefix"
    assert_eq!(
        output_record.get_field_by_index(vec![1].into()).unwrap(),
        &prefix
    );
    assert_eq!(
        output_record.get_field_by_index(vec![0].into()).unwrap(),
        &peer_ip
    );
    trace!("{:#?}", output_stream_queue);
    assert_eq!(accept_reject, AcceptReject::Accept);
}

#[test]
fn test_routes_5() {
    common::init();
    let src = r#"
        filter rib-in-pre-filter {
            define {
                rx route: Route;
                context ctx: RouteContext;

                msg = Message {
                    text: "Some text",
                    data: {
                        pfx: route.prefix,
                        peer_ip: ctx.provenance.peer-id.addr
                    }
                };
            }
        
            term always {
                match {
                    msg.data.pfx.exists();
                }
            }
        
            // A typed named record
            action send_msg {
                mqtt.send(msg);
            }
        
            apply {
                filter match always matching {
                    send_msg;
                    return accept;
                };
                reject;
            }
        }
        
        output-stream mqtt contains Message {
            text: String, 
            data: { 
                pfx: Prefix,
                peer_ip: IpAddress
            }
        }
    "#;

    let test_run = test_data(Filter("rib-in-pre-filter".into()), src);

    let (
        VmResult {
            accept_reject,
            output_stream_queue,
            ..
        },
        prefix,
        peer_ip,
    ) = test_run.unwrap();
    let output_record = output_stream_queue[0].get_record();
    trace!("{:#?}", output_record);
    assert_eq!(output_stream_queue.len(), 1);

    // The outputted record is an ordered_record, meaning it is sorted
    // alphabetically on the key, so [0] is "peer_ip" and [1] is "prefix"
    trace!(
        "output_rec {:?}",
        output_record.get_field_by_index(vec![0, 1].into())
    );
    assert_eq!(
        output_record.get_field_by_index(vec![0, 1].into()).unwrap(),
        &prefix
    );
    assert_eq!(
        output_record.get_field_by_index(vec![0, 0].into()).unwrap(),
        &peer_ip
    );
    trace!("{:#?}", output_stream_queue);
    assert_eq!(accept_reject, AcceptReject::Accept);
}

#[test]
fn test_routes_6() {
    common::init();
    let src = r#"
        filter rib-in-pre-filter {
            define {
                rx route: Route;
                context ctx: RouteContext;

                msg = Message {
                    text: "Some text",
                    data: {
                        pfx: route.prefix,
                        peer_ip: ctx.provenance.peer-id.addr
                    }
                };
            }
        
            term always {
                match {
                    msg.data.pfx.exists();
                }
            }
        
            // A typed named record
            action send_msg {
                mqtt.send(msg);
            }
        
            apply {
                filter match always matching {
                    send_msg;
                    return accept;
                };
                reject;
            }
        }
        
        output-stream mqtt contains Message {
            text: String, 
            data: { 
                pfx: Prefix,
                peer_ip: IpAddress
            }
        }
    "#;

    let test_run = test_data(Filter("rib-in-pre-filter".into()), src);

    let (
        VmResult {
            accept_reject,
            output_stream_queue,
            rx,
            ..
        },
        prefix,
        peer_ip,
    ) = test_run.unwrap();
    let output_record = output_stream_queue[0].get_record();
    trace!("{:#?}", output_record);
    assert_eq!(output_stream_queue.len(), 1);

    // The outputted record is an ordered_record, meaning it is sorted
    // alphabetically on the key, so [0] is "peer_ip" and [1] is "prefix"
    trace!(
        "output_rec {:?}",
        output_record.get_field_by_index(vec![0, 1].into())
    );
    assert_eq!(
        output_record.get_field_by_index(vec![0, 1].into()).unwrap(),
        &prefix
    );
    assert_eq!(
        output_record.get_field_by_index(vec![0, 0].into()).unwrap(),
        &peer_ip
    );

    let route = rx.clone().into_prefix_route().unwrap();
    // assert_eq!(route.afi_safi(), AfiSafi::Ipv6Unicast);

    let next_hop = route
        .get_field_by_index(&FieldIndex::from(vec![
            1,
            BasicRouteToken::NextHop.into(),
        ]))
        .unwrap();
    trace!("next hop in route {:?}", next_hop);

    assert_eq!(
        next_hop,
        TypeValue::Builtin(BuiltinTypeValue::NextHop(NextHop::Ipv6LL(
            std::net::Ipv6Addr::new(
                0xfc00, 0x10, 0x01, 0x10, 0x0, 0x0, 0x0, 0x10
            ),
            std::net::Ipv6Addr::new(
                0xfe80, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x10
            )
        )))
    );
    trace!("{:#?}", output_stream_queue);
    assert_eq!(accept_reject, AcceptReject::Accept);
}


#[test]
fn test_create_pdu_from_rws() {
    common::init();

    let mut rws1 = RouteWorkshop::new(Ipv6UnicastNlri::from_str(
        "2001:fe80:2d::/48"
    ).unwrap());

    rws1.set_attr::<LocalPref>(LocalPref(80)).unwrap();
    let nlri: Ipv6UnicastNlri = *rws1.nlri();
    
    let mut new_update_pdu = UpdateBuilder::<bytes::BytesMut, Ipv6UnicastNlri>::from_workshop(rws1);
    new_update_pdu.add_announcement(nlri).unwrap();
}