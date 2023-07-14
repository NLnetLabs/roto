use roto::compile::Compiler;

use roto::types::builtin::{RawRouteWithDeltas, RotondaId, UpdateMessage, Prefix, Asn, RouteStatus};
use roto::types::collections::Record;
use roto::types::typedef::TypeDef;
use roto::vm;
use routecore::bgp::message::SessionConfig;

fn test_data(
    name: &str,
    source_code: &'static str,
) -> Result<(), Box<dyn std::error::Error>> {
    println!("Evaluate module {}...", name);

    env_logger::init();

    // Compile the source code in this example
    let rotolo = Compiler::build(source_code)?;
    let roto_pack = rotolo.retrieve_public_as_refs(name)?;

    // BGP UPDATE message containing MP_REACH_NLRI path attribute,
    // comprising 5 IPv6 NLRIs
    let _buf = bytes::Bytes::from(vec![
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

    let _update: UpdateMessage =
        UpdateMessage::new(_buf, SessionConfig::modern());
    let _prefixes: Vec<Prefix> =
            _update.0.nlris().iter().filter_map(|n| n.prefix().map(|p| p.into())).collect();
    let _payload: RawRouteWithDeltas = RawRouteWithDeltas::new_with_message(
        (RotondaId(0), 0),
        _prefixes[0],
        _update,
        RouteStatus::InConvergence,
    );


    let payload_type = TypeDef::new_record_type(vec![("asn", Box::new(TypeDef::Asn))])?;
    let payload = Record::create_instance(&payload_type, vec![("asn", Asn::from(65534_u32).into())])?;
    // Create the VM
    println!("Used Arguments");
    println!("{:#?}", &roto_pack.get_arguments());
    println!("Used Data Sources");
    println!("{:#?}", &roto_pack.get_data_sources());

    // let filter_map_arguments = vec![(
    //     "extra_asn".into(),
    //     // use Roto type coercion
    //     TypeValue::from(65534_u32)
    // )];

    let ds_ref = roto_pack.get_data_sources();
    // let args = rotolo.compile_arguments(name, filter_map_arguments)?;

    let mut vm = vm::VmBuilder::new()
        // .with_arguments(args)
        .with_data_sources(ds_ref)
        .with_mir_code(roto_pack.get_mir())
        .build()?;

    let mem = &mut vm::LinearMemory::uninit();
    let res = vm.exec(
        payload,
        None::<Record>,
        // Some(filter_map_arguments),
        None,
        mem,
    )
    .unwrap();

    println!("\nRESULT");
    println!("action: {}", res.accept_reject);
    println!("rx    : {:?}", res.rx);
    println!("tx    : {:?}", res.tx);

    Ok(())
}

fn main() {
    test_data(
        "my-filter-map",
        r###"
            filter-map my-filter-map {
                define {
                    // specify the types of that this filter receives
                    // and sends.
                    rx_tx pph_asn: MyRec;
                }

                term peer-asn-matches {
                    match {
                        pph_asn.asn == AS65534;
                    }
                }

                apply {
                    filter match peer-asn-matches matching { return accept; };
                    return reject;
                }
            }

            type MyRec {
                asn: Asn
            }
        "###,
    ).unwrap();
}
