use std::str::FromStr;

use log::trace;
use roto::{blocks::Scope, compiler::Compiler, types::{builtin::{UpdateMessage, RotondaId, RawRouteWithDeltas, RouteStatus}, collections::Record}, vm::{VmResult, self}, types::builtin::Prefix, ast::AcceptReject};
use routecore::bgp::message::SessionConfig;
use routes::bmp::encode::{mk_bgp_update, mk_per_peer_header, Prefixes, Announcements};

mod common;

fn test_data(
    name: Scope,
    source_code: &str,
    announce_str: &str
) -> Result<(VmResult, RawRouteWithDeltas), Box<dyn std::error::Error>> {
    common::init();
    println!("Evaluate filter-map {}...", name);

    // Compile the source code in this example
    let rotolo = Compiler::build(source_code)?;
    let roto_pack = rotolo.retrieve_pack_as_arcs(&name)?;

    // Create a BGP packet
    let prefix_str = "192.0.2.1";
    let per_peer_header = mk_per_peer_header(prefix_str, 1);
    let withdrawals = Prefixes::default();
    let announcements = Announcements::from_str(announce_str).unwrap();

    let msg_buf = mk_bgp_update(
        &per_peer_header,
        &withdrawals,
        &announcements,
        &[],
    );

    let bgp_msg = UpdateMessage::new(msg_buf.0, SessionConfig::modern())?;

    let payload = RawRouteWithDeltas::new_with_message(
        (RotondaId(0),0),
        Prefix::new(routecore::addr::Prefix::from_str("192.0.2.0/24")?),
        bgp_msg,
        RouteStatus::UpToDate
    )?;

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
    let res = vm
        .exec(
            payload.clone(),
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

//------------ Test: AS PATHS ------------------------------------------------

#[test]
fn test_as_path_1() {
    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res,_) = test_data(Scope::Filter("test".into()),
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
     annc
    ).unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn test_as_path_2() {
    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res,_) = test_data(Scope::Filter("test".into()),
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
     annc
    ).unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn test_as_path_3() {
    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res,_) = test_data(Scope::Filter("test".into()),
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
     annc
    ).unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn test_as_path_4() {
    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res,_) = test_data(Scope::Filter("test".into()),
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
     annc
    ).unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn test_as_path_5() {
    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res,_) = test_data(Scope::Filter("test".into()),
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
     annc
    ).unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn test_as_path_6() {
    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res,_) = test_data(Scope::Filter("test".into()),
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
     annc
    ).unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn test_as_path_7() {
    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res,_) = test_data(Scope::Filter("test".into()),
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
     annc
    ).unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}



#[test]
#[should_panic = r#"Result::unwrap()` on an `Err` value: "Eval error: Unknown method 'the_unknown_method' for type AsPath"#]
fn test_as_path_8() {
    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res,_) = test_data(Scope::Filter("test".into()),
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
     annc
    ).unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

//------------ Test: Communities ---------------------------------------------

#[test]
fn test_std_comms_1() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res,_) = test_data(Scope::Filter("test".into()),
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
     annc
    ).unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn test_std_comms_2() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res,_) = test_data(Scope::Filter("test".into()),
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
     annc
    ).unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn test_std_comms_3() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res,_) = test_data(Scope::Filter("test".into()),
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
     annc
    ).unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
#[should_panic]
fn test_std_comms_4() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res,_) = test_data(Scope::Filter("test".into()),
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
     annc
    ).unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
#[should_panic]
fn test_std_comms_5() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,123:44 192.0.2.0/24";
    let (res,_) = test_data(Scope::Filter("test".into()),
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
     annc
    ).unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
fn test_ext_comms_1() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,rt:123:44 192.0.2.0/24";
    let (res,_) = test_data(Scope::Filter("test".into()),
     r#"
     filter test {
        define {
            rx route: Route;
        }

        term test {
            match {
                route.communities.contains(rt:123:44);
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
     annc
    ).unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Accept);
}

#[test]
fn test_ext_comms_2() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,rt:123:44 192.0.2.0/24";
    let (res,_) = test_data(Scope::Filter("test".into()),
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
     annc
    ).unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}

#[test]
#[should_panic = r#"Result::unwrap()` on an `Err` value: "Eval error: Cannot convert literal 'XOTOR:4500:34' into Extended Community: unknown tag"#]
fn test_ext_comms_3() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,rt:123:44 192.0.2.0/24";
    let (res,_) = test_data(Scope::Filter("test".into()),
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
     annc
    ).unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}


#[test]
#[should_panic = r#"Result::unwrap()` on an `Err` value: "Eval error: Cannot convert literal 'rt:450076876500:34' into Extended Community: invalid rt:AS:AN"#]
fn test_ext_comms_4() {
    common::init();

    let annc = "e [123,456,789] 10.0.0.1 BLACKHOLE,rt:123:44 192.0.2.0/24";
    let (res,_) = test_data(Scope::Filter("test".into()),
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
     annc
    ).unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}