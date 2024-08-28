#![cfg(any())]
use log::trace;
use roto::ast::AcceptReject;

use roto::blocks::Scope::{self, FilterMap};
use roto::pipeline;
use roto::types::builtin::{NlriStatus, PeerId, PeerRibType, Provenance, RouteContext};
use roto::types::collections::{BytesRecord, Record};
use roto::types::lazyrecord_types::BgpUpdateMessage;
use roto::types::typevalue::TypeValue;
use roto::vm::{self, VmResult};
use inetnum::asn::Asn;
use routecore::bgp::message::SessionConfig;

mod common;

fn test_data(
    name: Scope,
    source_code: &str,
) -> Result<(VmResult, BytesRecord<BgpUpdateMessage>), Box<dyn std::error::Error>> {
    println!("Evaluate filter-map {}...", name);

    // Compile the source code in this example
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

    // let update: UpdateMessage =
    //     UpdateMessage::new(buf.clone(), SessionConfig::modern());
    // let prefixes: Vec<Prefix> =
    //         update.0.nlris().iter().filter_map(|n| n.prefix().map(|p| p.into())).collect();
    // let msg_id = (RotondaId(0), 0);

    // let payload: RawRouteWithDeltas = RawRouteWithDeltas::new_with_message(
    //     msg_id,
    //     prefixes[0],
    //     update,
    // );
    let payload = 
        BytesRecord::<BgpUpdateMessage>::new(buf, SessionConfig::modern()).unwrap();

    // let payload2 = TypeValue::Builtin(
    //     roto::types::builtin::BuiltinTypeValue::BgpUpdateMessage(
    //         Arc::new(payload),
    //     ),
    // );

    // let payload = BgpUpdateMessage::new(msg_id, update);

    // assert!(roto_pack.check_rx_payload_type(&payload2));

    // Create the VM
    trace!("Used Arguments");
    trace!("{:#?}", &roto_pack.get_arguments());
    trace!("Used Data Sources");
    trace!("{:#?}", &roto_pack.get_data_sources());

    let ds_ref = roto_pack.get_data_sources();

    for mb in roto_pack.get_mir().iter() {
        println!("{}", mb);
    }

    let prov = Provenance {
        timestamp: chrono::Utc::now(),
        connection_id: "127.0.0.1:178".parse().unwrap(),
        peer_id: PeerId { addr: "127.0.0.1".parse().unwrap(), asn: Asn::from(65530)},
        peer_bgp_id: [0,0,0,0].into(),
        peer_distuingisher: [0; 8],
        peer_rib_type: PeerRibType::OutPost,
    };

    let context = &RouteContext::new(None, NlriStatus::Empty, prov);

    let mut vm = vm::VmBuilder::new()
        // .with_arguments(args)
        .with_data_sources(ds_ref)
        .with_context(context)
        .with_mir_code(roto_pack.get_mir())
        .build()?;

    let mem = &mut vm::LinearMemory::uninit();
    let res = vm
        .exec(
            TypeValue::from(payload.clone()),
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

    Ok((res, payload))
}

#[test]
fn test_bgp_update_1() {
    common::init();

    let (res, _payload) = test_data(
        Scope::FilterMap("filter-unicast-v4-v6-only".into()),
        r###"
        filter-map filter-unicast-v4-v6-only {
            define {
                rx_tx bgp_msg: BgpUpdateMessage;
                context ctx: RouteContext;
                // IPV4 = 100;
            }
        
            // AFI, u16,
            // 1 => Ipv4,
            // 2 => Ipv6,
            // 25 => L2Vpn,

            // SAFI, u8,
            // 1 => Unicast,
            // 2 => Multicast,
            // 4 => MplsUnicast,
            // 65 => Vpls,
            // 70 => Evpn,
            // 128 => MplsVpnUnicast,
            // 132 => RouteTarget,
            // 133 => FlowSpec,
            // 134 => FlowSpecVpn,

            term afi-safi-unicast {
                match {
                    ctx.provenance.peer-id.asn != AS64900;
                }
            }
        
            apply {
                filter match afi-safi-unicast matching {
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
fn test_bgp_update_2() {
    common::init();

    let res = test_data(
        FilterMap("filter-unicast-v4-v6-only".into()),
        r###"
        filter-map filter-unicast-v4-v6-only {
            define {
                rx_tx bgp_msg: BgpUpdateMessage;
                context ctx: RouteContext;
            }

            term afi-safi-unicast {
                match {
                    ctx.provenance.peer-id.asn == AS65530;
                }
            }
        
            apply {
                filter match afi-safi-unicast matching {
                    return accept;
                };
                reject;
            }
        }
        "###,
    )
    .unwrap();

    assert_eq!(res.0.accept_reject, AcceptReject::Accept);
}

#[test]
fn test_bgp_update_3() {
    common::init();
    let (res, payload) = test_data(
        Scope::FilterMap("bgp-update-filter-map-3".into()),
        r#"
        filter-map bgp-update-filter-map-3 {
            define {
                rx_tx bgp_msg: BgpUpdateMessage;
                context ctx: RouteContext;
            }
        
            term afi-safi-unicast {
                match {
                    ctx.provenance.peer-id.asn != AS64900;
                }
            }
        
            action send-message {
                bgp-msg.send({
                    name: "local-broker",
                    topic: "testing",
                    bgp_msg: bgp_msg
                });
            }
        
            apply {
                filter match afi-safi-unicast matching {
                    send-message;
                };
            }
        }
        
        output-stream bgp-msg contains Message2 {
            name: String,
            topic: String,
            bgp_msg: BgpUpdateMessage
        }
        "#,
    )
    .unwrap();

    for m in res.output_stream_queue.iter() {
        trace!("MESSAGE {:?}", m);
    }

    assert_eq!(res.output_stream_queue.len(), 1);
    assert_eq!(res.output_stream_queue[0].get_name(), "local-broker");
    assert_eq!(
        res.rx,
        TypeValue::Builtin(
            roto::types::builtin::BuiltinTypeValue::BgpUpdateMessage(payload,),
        )
    );
}
