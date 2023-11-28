use log::trace;
use roto::ast::AcceptReject;
use roto::compiler::Compiler;

use roto::blocks::Scope::{self};
use roto::types::builtin::{BgpUpdateMessage, RotondaId, UpdateMessage};
use roto::types::collections::Record;
use roto::vm::{self, VmResult};
use routecore::bgp::message::SessionConfig;

mod common;

fn test_data(
    name: Scope,
    source_code: &str,
) -> Result<(VmResult, BgpUpdateMessage), Box<dyn std::error::Error>> {
    println!("Evaluate filter {}...", name);

    // Compile the source code in this example
    let rotolo = Compiler::build(source_code)?;
    let roto_pack = rotolo.retrieve_pack_as_arcs(&name)?;

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

    let msg_id = (RotondaId(0), 0);

    let payload = BgpUpdateMessage::new(
        msg_id,
        UpdateMessage::new(buf, SessionConfig::modern()).unwrap(),
    );

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
#[should_panic(expected="Eval error: Duplicate FilterMap with name \
'bmp-in-filter'")]
fn test_two_filters_1() {
    common::init();

    let (_res, _payload) = test_data(
        Scope::Filter("bmp-in-filter".into()),
        r###"
        filter bmp-in-filter {
            define {
                rx msg: BmpMessage;
            }
        
            apply {
                reject;
            }
        }

        filter bmp-in-filter {
            define {
                rx msg: BmpMessage;
            }
        
            apply {
                reject;
            }
        }
        "###,
    )
    .unwrap();
}

#[test]
fn test_two_filters_2() {
    common::init();

    let (res, _payload) = test_data(
        Scope::Filter("bmp-in-filter".into()),
        r###"
        filter bmp-in-filter {
            define {
                rx msg: BmpMessage;
            }
        
            apply {
                reject;
            }
        }

        filter bmp-out-filter {
            define {
                rx msg: BmpMessage;
            }
        
            apply {
                reject;
            }
        }
        "###,
    )
    .unwrap();

    assert_eq!(res.accept_reject, AcceptReject::Reject);
}
