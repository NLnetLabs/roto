use roto::compile::Compiler;

use roto::types::builtin::{
    Asn, Community,
};
use roto::types::collections::{ElementTypeValue, List, Record};
use roto::types::typedef::TypeDef;
use roto::types::typevalue::TypeValue;
use roto::vm;

mod common;

fn test_data(
    name: &str,
    source_code: &'static str,
) -> Result<(), Box<dyn std::error::Error>> {
    println!("Evaluate module {}...", name);

    let module_arguments = vec![(
        "my_asn",
        TypeValue::from(Asn::from(65534_u32))
    )];

    let mut c = Compiler::new();
    c.with_arguments(name, module_arguments)?;
    let roto_packs = c.build_from_compiler(source_code)?;

    println!("miscompilations");
    println!("{:?}", roto_packs.get_mis_compilations());
    let roto_pack = roto_packs.retrieve_public_as_arcs(name)?;

    let _count: TypeValue = 1_u32.into();
    let prefix: TypeValue =
        routecore::addr::Prefix::new("193.0.0.0".parse().unwrap(), 24)?
            .into();
    let next_hop: TypeValue =
        std::net::IpAddr::V4(std::net::Ipv4Addr::new(193, 0, 0, 23)).into();
    let as_path = vec![Asn::from_u32(1)].into();
    let asn: TypeValue = Asn::from_u32(211321).into();

    println!("{:?}", asn);

    let comms =
        TypeValue::List(List::new(vec![ElementTypeValue::Primitive(
            Community::new(routecore::bgp::communities::Community::from([
                127, 12, 13, 12,
            ]))
            .into(),
        )]));

    let my_comms_type =
        TypeDef::List(Box::new(TypeDef::List(Box::new(TypeDef::Community))));

    let my_nested_rec_type =
        TypeDef::new_record_type(vec![("counter", Box::new(TypeDef::U32))])
            .unwrap();

    let _my_nested_rec_instance = Record::create_instance(
        &my_nested_rec_type,
        vec![(
            "counter",
            1_u32.into(),
        )],
    )
    .unwrap();

    let my_rec_type = TypeDef::new_record_type(vec![
        ("prefix", Box::new(TypeDef::Prefix)),
        ("as-path", Box::new(TypeDef::AsPath)),
        ("origin", Box::new(TypeDef::Asn)),
        ("next-hop", Box::new(TypeDef::IpAddress)),
        ("med", Box::new(TypeDef::U32)),
        ("local-pref", Box::new(TypeDef::U32)),
        ("communities", Box::new(my_comms_type)),
    ])
    .unwrap();

    let my_payload = Record::create_instance(
        &my_rec_type,
        vec![
            ("prefix", prefix),
            ("as-path", as_path),
            ("origin", asn),
            ("next-hop", next_hop),
            ("med", 80_u32.into()),
            ("local-pref", 20_u32.into()),
            ("communities", comms),
        ],
    )
    .unwrap();

    let mem = &mut vm::LinearMemory::uninit();

    println!("Used Arguments");
    println!("{:#?}", &roto_pack.arguments);
    println!("Used Data Sources");
    println!("{:#?}", &roto_pack.data_sources);

    let ds_ref = roto_pack.data_sources.iter().collect::<Vec<_>>();

    println!("Start vm...");
    let mut vm = vm::VmBuilder::new()
        // .with_arguments(args)
        .with_data_sources(ds_ref)
        .with_mir_code(roto_pack.mir)
        .build();

    let res = vm
        .exec(my_payload, None::<Record>, None, mem)
        .unwrap();

    println!("\nRESULT");
    println!("action: {}", res.0);
    println!("rx    : {:?}", res.1);
    println!("tx    : {:?}", res.2);

    Ok(())
}

#[test]
fn test_module_message() {
    common::init();
    test_data(
        "my-message-module",
        r###"
        module my-message-module with my_asn: Asn {
            define {
                // specify the types of that this filter receives
                // and sends.
                // rx_tx route: StreamRoute;
                rx route: Route;
                tx out: Route;
            }

            term rov-valid for route: Route {
                match {
                    route.as-path.origin() == my_asn;
                }
            }
            
            action send-message {
                mqtt.send({ 
                    message: String.format("🤭 I encountered {}", my_asn),  
                    my_asn: my_asn
                });
            }

            apply {
                filter match rov-valid not matching {  
                    send-message;
                };
            }
        }

        output-stream mqtt contains Message {
             message: String,
             asn: Asn
        }
        "###,
    ).unwrap();
}
