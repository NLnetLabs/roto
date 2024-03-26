use std::str::FromStr;

use log::trace;
use roto::{
    ast::AcceptReject,
    blocks::Scope,
    compiler::Compiler,
    types::{
        builtin::{
            basic_route::{BasicRoute, PeerId, PeerRibType, Provenance}, NlriStatus, RouteContext,
        }, collections::{BytesRecord, Record}, lazyrecord_types::BgpUpdateMessage, typevalue::TypeValue
    },
    vm::{self, VmResult},
};
use inetnum::asn::Asn;
use routecore::bgp::message::SessionConfig;

use routes::bmp::encode::{
    mk_bgp_update, mk_per_peer_header, Announcements, Prefixes,
};

mod common;

fn test_data(
    name: Scope,
    source_code: &str,
    announce_str: &str,
) -> Result<(VmResult, BasicRoute), Box<dyn std::error::Error>> {
    common::init();
    println!("Evaluate filter-map {}...", name);

    // Compile the source code in this example
    let rotolo = Compiler::build(source_code)?;
    let roto_pack = rotolo.retrieve_pack_as_refs(&name)?;

    // Create a BGP packet
    let prefix_str = "192.0.2.1";
    let per_peer_header = mk_per_peer_header(prefix_str, 65535);
    let withdrawals = Prefixes::default();
    let announcements = Announcements::from_str(announce_str).unwrap();

    let msg_buf =
        mk_bgp_update(&per_peer_header, &withdrawals, &announcements, &[]);

    let bgp_msg = BytesRecord::<BgpUpdateMessage>::new(msg_buf.0, SessionConfig::modern())?;
    let afi_safis = bgp_msg.bytes_parser().afi_safis().into_iter().flatten();

    let prov = Provenance {
        timestamp: chrono::Utc::now(),
        connection_id: "127.0.0.1:8080".parse().unwrap(),
        peer_id: PeerId { addr: "172.0.0.1".parse().unwrap(), asn: Asn::from(65530)},
        peer_bgp_id: [0,0,0,0].into(),
        peer_distuingisher: [0; 8],
        peer_rib_type: PeerRibType::OutPost,
    };

    let parser = bgp_msg.bytes_parser();
    // let pa_map = PaMap::from_update_pdu(parser).unwrap();

    // let parser = Parser::from_ref(parser.octets());
    let rws = &explode_into_wrapped_rws_vec::<'_, _, bytes::Bytes, TypeValue>(afi_safis, false, parser).unwrap()[0];

    let context = RouteContext::new(
        // routecore::addr::Prefix::from_str("192.0.2.0/24")?,
        Some(bgp_msg.clone()),
        // routecore::bgp::types::AfiSafi::Ipv4Unicast,
        // None,
        // nlri.clone(),
        NlriStatus::UpToDate,
        prov
    );

    // let payload = RouteWorkshop::from_update_pdu(
    //     nlri,
    //     &bgp_msg.into_inner()
    // )?;

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
        .with_context(&context)
        .with_mir_code(roto_pack.get_mir())
        .build()?;
    
    let mem = &mut vm::LinearMemory::uninit();
    let res = vm
        .exec(
            // roto::types::builtin::BasicRoute::new(payload.clone()),
            rws.clone(),
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

    Ok((res, rws.clone().into_route().unwrap()))
}

//------------ Test: IpAddressLiteral ----------------------------------------

#[test]
fn test_ip_address_literal_1() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.prefix.address() == 192.0.3.0;
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn test_ip_address_literal_2() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.prefix.address() == 192.0.2.0;
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn test_ip_address_literal_3() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let res = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.prefix.address() == 192.0.2.0.34.456.;
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    );

    assert!(res.is_err());
    trace!("res {:?}", res);
    assert!(format!("{}", res.err().unwrap()).starts_with(r#"Parse error"#));
}

#[test]
fn test_ip_address_literal_4() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.prefix.address() == 2001::1;
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn test_ip_address_literal_5() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.prefix.address() == 2001:fa00::1;
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn test_ip_address_literal_6() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.prefix.address() == ::;
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn test_ip_address_literal_7() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.prefix.address() == 2001:fa80:123::1;
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

//------------ Test: PrefixLiteral -------------------------------------------

#[test]
fn test_prefix_literal_1() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.prefix == 192.0.2.0/24;
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn test_prefix_literal_2() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.prefix == 10.0.0.0/8;
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn test_prefix_literal_3() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                192.0.0.0/16.covers(route.prefix);
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn test_prefix_literal_4() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.prefix.is_covered_by(192.0.0.0/16);
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn test_prefix_literal_5() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.prefix == ::/48;
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn test_prefix_literal_6() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.prefix == 2001::/48;
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn test_prefix_literal_7() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.prefix == 2001:ee34:23a0:123::/64;
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn test_prefix_literal_8() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.prefix.contains(192.0.2.254);
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn test_prefix_literal_9() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.prefix.contains(192.0.3.254);
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
#[should_panic = r#"Eval error: Unknown method: 'blaffs' for type Prefix"#]
fn test_prefix_literal_10() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.prefix.blaffs(192.0.3.254);
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn test_prefix_literal_11() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
            pfx = Prefix.from(192.0.2.128,/25);
        }

        term test {
            match {
                route.prefix.covers(pfx);
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn test_prefix_literal_12() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
            pfx = Prefix.from(192.2.0.0,/16);
        }

        term test {
            match {
                route.prefix.covers(pfx);
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

//------------ Test: As Paths ------------------------------------------------

#[test]
fn test_as_path_1() {
    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.as-path.contains(AS300);
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn test_as_path_2() {
    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.as-path.contains(AS456);
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn test_as_path_3() {
    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.as-path.origin() == AS456;
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn test_as_path_4() {
    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.as-path.origin() == AS789;
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn test_as_path_5() {
    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.as-path.origin() == AS123;
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn test_as_path_6() {
    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.as-path.len() == 3;
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn test_as_path_7() {
    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.as-path.len() == 12;
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
#[should_panic = r#"Result::unwrap()` on an `Err` value: "Eval error: Unknown method 'the_unknown_method' for type AsPath"#]
fn test_as_path_8() {
    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.as-path.the_unknown_method();
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

//------------ Test: Communities ---------------------------------------------

#[test]
fn test_std_comms_1() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.communities.contains(123:44);
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn test_std_comms_2() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.communities.contains(123:45);
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn test_std_comms_3() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.communities.contains(122:45);
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn test_std_comms_7() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                // (0x7b, 0x2c) = (123, 44)
                route.communities.contains(0x7b:0x2c);
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
#[should_panic]
fn test_std_comms_4() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                // The tag of the community is too big!
                route.communities.contains(7500:450000);
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
#[should_panic]
fn test_std_comms_5() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                // The ASN of the community is too big!
                route.communities.contains(75003498:450);
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn test_ext_comms_1() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,rt:123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.communities.contains(rt:0x7b:0x2c);
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn test_ext_comms_2() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,rt:123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.communities.contains(750:4500);
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
#[should_panic = r#"Result::unwrap()` on an `Err` value: "Eval error: Cannot convert literal 'XOTOR:4500:34' into Extended Community: unknown tag"#]
fn test_ext_comms_3() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,rt:123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.communities.contains(XOTOR:4500:34);
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
#[should_panic = r#"Result::unwrap()` on an `Err` value: "Eval error: Cannot convert literal 'rt:450076876500:34' into Extended Community: invalid rt:AS:AN"#]
fn test_ext_comms_4() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,rt:123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.communities.contains(rt:450076876500:34);
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn test_l_comms_1() {
    common::init();

    let annc =
        "e [123,456,789] 10.0.0.1 BLACKHOLE,AS65536:123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.communities.contains(AS65536:123:44);
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn test_l_comms_2() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,65536:123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.communities.contains(AS65536:123:44);
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn test_l_comms_3() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,65536:123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.communities.contains(AS10:123:44);
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn test_wk_comms_1() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,65536:123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.communities.contains(BLACKHOLE);
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn test_wk_comms_2() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,65536:123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                // 0xFFFF029A is the binary value (two octets) for BLACKHOLE
                route.communities.contains(0xFFFF029A);
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn test_wk_comms_3() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,65536:123:44 192.0.2.0/24";
    let (res, _) = test_data(
        Scope::Filter("test".into()),
        r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                // This binary value is turned into an extended community,
                // it will never match BLACKHOLE
                route.communities.contains(0xFFFF029A0000);
            }
        }
    
        apply {
            filter match test matching {
                return accept;
            };
            reject;
        }
    }
    "#,
        annc,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}
