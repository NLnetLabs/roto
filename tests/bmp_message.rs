use std::sync::Arc;

use log::trace;
use roto::{
    types::{builtin::{
       BytesRecord,
    }, lazytypedef::RouteMonitoring, typevalue::TypeValue, collections::Record},
    vm::{VmError, self}, compile::Compiler, ast::AcceptReject,
};

mod common;

fn test_data(
    name: &str,
    source_code: &'static str,
) -> Result<(AcceptReject, TypeValue, Option<TypeValue>), Box<dyn std::error::Error>> {    
    println!("Evaluate module {}...", name);

    // Compile the source code in this example
    let rotolo = Compiler::build(source_code)?;
    let roto_pack = rotolo.retrieve_public_as_arcs(name)?;

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

    let rm_msg = BytesRecord::<RouteMonitoring>::new(buf.into());
    assert!(rm_msg.is_ok());
    let rm_msg = rm_msg.unwrap();
    let payload = TypeValue::Builtin(roto::types::builtin::BuiltinTypeValue::BmpRouteMonitoringMessage(Arc::new(rm_msg)));

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

    Ok(res)
}

#[test]
fn bmp_message_1() {
    common::init();

    let res = test_data(
        "filter-v4-only",
        r###"
        module filter-v4-only {
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
    ).unwrap();

    assert_eq!(res.0, AcceptReject::Accept);
}

#[test]
fn bmp_message_2() {
    common::init();

    let res = test_data(
        "filter-v6-only",
        r###"
        module filter-v6-only {
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
    ).unwrap();

    assert_eq!(res.0, AcceptReject::Reject);
}

#[test]
fn bmp_message_3() {
    common::init();

    let res = test_data(
        "filter-v6-only",
        r###"
        module filter-v6-only {
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

    let err = "Eval error: Cannot convert value with type Lazy Record".to_string();
    let mut str = res.unwrap_err().to_string();
    str.truncate(err.len());
    assert_eq!(str, err);
}