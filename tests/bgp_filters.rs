use std::str::FromStr;

use log::trace;
use roto::{blocks::Scope, compiler::{CompileError, Compiler}, types::{builtin::{BgpUpdateMessage, UpdateMessage, RotondaId, RawRouteWithDeltas, RouteStatus}, collections::Record, typevalue::TypeValue}, vm::{VmResult, self}, types::builtin::Prefix, ast::AcceptReject};
use routecore::bgp::{message::SessionConfig, self};
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