use log::trace;
use roto::compile::Compiler;

use roto::types::builtin::{RawRouteWithDeltas, RotondaId, UpdateMessage, Prefix, Asn, BgpUpdateMessage};
use roto::types::collections::Record;
use roto::types::typedef::TypeDef;
use roto::vm;
use routecore::bgp::message::SessionConfig;

mod common;

fn test_data(
    name: &str,
    source_code: &'static str,
) -> Result<(), Box<dyn std::error::Error>> {
    println!("Evaluate module {}...", name);

    // Compile the source code in this example
    let rotolo = Compiler::build(source_code)?;
    let roto_pack = rotolo.retrieve_public_as_arcs(name)?;

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
    // let prefixes: Vec<Prefix> =
    //         update.0.nlris().iter().filter_map(|n| n.prefix().map(|p| p.into())).collect();
    let msg_id = (RotondaId(0), 0);

    // let payload: RawRouteWithDeltas = RawRouteWithDeltas::new_with_message(
    //     msg_id,
    //     prefixes[0],
    //     update,
    // );
    let payload = BgpUpdateMessage::new(msg_id, update);

    // Create the VM
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
    let res = vm.exec(
        payload,
        None::<Record>,
        // Some(module_arguments),
        None,
        mem,
    )
    .unwrap();

    trace!("\nRESULT");
    trace!("action: {}", res.0);
    trace!("rx    : {:?}", res.1);
    trace!("tx    : {:?}", res.2);

    Ok(())
}

#[test]
fn test_bgp_update_1() {
    common::init();

    test_data(
        "filter-unicast-v4-v6-only",
        r###"
        module filter-unicast-v4-v6-only {
            define {
                rx_tx bgp_msg: BgpUpdateMessage;
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
                    // bgp_msg.nlris.afi in [AFI_IPV4, AFI_IPV6];
                    // bgp_msg.nlris.afi in [Ipv4, Ipv6];
                    // bgp_msg.nlris.afi in [AFI.Ipv4, AFI.Ipv6];
                    bgp_msg.nlris.afi in [IPV4, IPV6];
                    // bgp_msg.nlris.afi in [afi.IPV4, afi.IPV6];
                    // bgp_msg.nlris.afi in IPV4 | IPV6;
                    bgp_msg.nlris.safi == UNICAST;
                    // IPV4 == 150;
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
    ).unwrap();
}
