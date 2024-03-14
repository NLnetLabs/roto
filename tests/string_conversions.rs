use log::trace;
use roto::compiler::Compiler;

use roto::blocks::Scope;
use roto::types::builtin::{BuiltinTypeValue, NlriStatus, PeerId, PeerRibType, Provenance, RouteContext};
use roto::types::collections::{ElementTypeValue, List, Record};
use roto::types::enum_types::EnumVariant;
use roto::types::typedef::TypeDef;
use roto::types::typevalue::TypeValue;
use roto::vm::{self, VmResult};

use routecore::bgp::communities::HumanReadableCommunity as Community;
use routecore::asn::Asn;

mod common;

fn src_code(format_line: &str) -> String {
    let pre = format!(
        r###"
        filter my-message-filter-map-5 with my_asn: Asn {{
            define {{
                // specify the types of that this filter receives
                // and sends.
                // rx_tx route: StreamRoute;
                rx route: MyPayload;
            }}

            term rov-valid for route: Route {{
                match {{
                    route.as-path.origin() == my_asn;
                }}
            }}
            
            action send-message {{
                mqtt.send({{ 
                    name: "My ASN",
                    topic: "My Asn was Seen!",
                    asn: my_asn,
                    message: {}
                }});
            }}

            apply {{
                filter match rov-valid not matching {{
                    send-message;
                }};
            }}
        }}

        output-stream mqtt contains Message {{
            asn: Asn,
            message: String,
            name: String,
            topic: String
        }}

        type MyPayload {{
            prefix: Prefix,
            as-path: AsPath,
            bmp_msg_type: BmpMessageType,
            origin: Asn,
            next-hop: IpAddress,
            med: U32,
            local-pref: U32,
            communities: [Community]
        }}
    "###,
        format_line
    );

    pre
}

fn test_data(
    name: Scope,
    source_code: &str,
) -> Result<VmResult, Box<dyn std::error::Error>> {
    println!("Evaluate filter-map {}...", name);

    let filter_map_arguments =
        vec![("my_asn", TypeValue::from(Asn::from(65534_u32)))];

    let mut c = Compiler::new();
    c.with_arguments(&name, filter_map_arguments)?;
    let roto_packs = c.build_from_compiler(source_code)?;

    println!("miscompilations");
    println!("{:?}", roto_packs.get_mis_compilations());
    let roto_pack = roto_packs.retrieve_pack_as_refs(&name)?;

    let _count: TypeValue = 1_u32.into();
    let prefix: TypeValue =
        routecore::addr::Prefix::new("193.0.0.0".parse().unwrap(), 24)?
            .into();
    let next_hop: TypeValue =
        std::net::IpAddr::V4(std::net::Ipv4Addr::new(193, 0, 0, 23)).into();
    let as_path = vec![Asn::from_u32(65534), Asn::from_u32(65335)].into();
    let asn: TypeValue = Asn::from_u32(211321).into();

    println!("{:?}", asn);

    let comms = TypeValue::List(List::new(vec![
        ElementTypeValue::Primitive(
            Community::from([
                127, 12, 13, 12,
            ]).into()),
            ElementTypeValue::Primitive(
                Community::from([
                    127, 12, 13, 20,
                ]).into())
        ])
    );

    let my_comms_type = (&comms).into();

    let my_bmp_msg_type_instance =
        TypeValue::Builtin(BuiltinTypeValue::ConstU8EnumVariant(
            EnumVariant::<u8>::new(("BMP_MESSAGE_TYPE".into(), 1)),
        ));
    let my_bmp_msg_type_type = (&my_bmp_msg_type_instance).into();

    let my_rec_type = TypeDef::new_record_type(vec![
        ("prefix", Box::new(TypeDef::Prefix)),
        ("as-path", Box::new(TypeDef::AsPath)),
        ("origin", Box::new(TypeDef::Asn)),
        ("next-hop", Box::new(TypeDef::IpAddr)),
        ("med", Box::new(TypeDef::U32)),
        ("local-pref", Box::new(TypeDef::U32)),
        ("communities", Box::new(my_comms_type)),
        ("bmp_msg_type", Box::new(my_bmp_msg_type_type)),
    ])
    .unwrap();

    let my_payload = Record::create_instance_with_sort(
        &my_rec_type,
        vec![
            ("prefix", prefix),
            ("as-path", as_path),
            ("origin", asn),
            ("next-hop", next_hop),
            ("med", 80_u32.into()),
            ("local-pref", 20_u32.into()),
            ("communities", comms),
            ("bmp_msg_type", my_bmp_msg_type_instance),
        ],
    )
    .unwrap();

    let mem = &mut vm::LinearMemory::uninit();

    println!("Used Arguments");
    println!("{:#?}", &roto_pack.arguments);
    println!("Used Data Sources");
    println!("{:#?}", &roto_pack.data_sources);

    for mb in roto_pack.get_mir().iter() {
        println!("{}", mb);
    }

    let my_payload = TypeValue::Record(my_payload);
    assert!(roto_pack.check_rx_payload_type(&my_payload));

    let ds_ref = roto_pack.data_sources;

    let peer_ip = "192.0.2.0".parse().unwrap();

    let provenance = Provenance {
        timestamp: chrono::Utc::now(),
        connection_id: "192.0.2.0:178".parse().unwrap(),
        peer_id: PeerId { addr: peer_ip, asn: Asn::from(65534) },
        peer_bgp_id: [0; 4].into(),
        peer_distuingisher: [0; 8],
        peer_rib_type: PeerRibType::OutPost,
    };

    let context = RouteContext::new(None, NlriStatus::InConvergence, provenance);

    println!("Start vm...");
    let mut vm = vm::VmBuilder::new()
        // .with_arguments(args)
        .with_context(context)
        .with_data_sources(ds_ref)
        .with_mir_code(roto_pack.mir)
        .build()?;

    let res = vm.exec(my_payload, None::<Record>, None, mem).unwrap();

    println!("\nRESULT");
    println!("action: {}", res.accept_reject);
    println!("rx    : {:?}", res.rx);
    println!("tx    : {:?}", res.tx);
    println!("stream: {:?}", res.output_stream_queue);

    Ok(res)
}

#[test]
fn test_filter_map_message_00() {
    common::init();

    let code_line = src_code(
        r#"String.format("🤭 I, the messager, saw {} in a BGP update.", AS3200)"#,
    );

    let res = test_data(
        Scope::Filter("my-message-filter-map-5".into()),
        &code_line,
    );

    // let err = "Eval error: Record {message: String, my_asn: Asn, } cannot"
    //     .to_string();
    // let mut str = res.unwrap_err().to_string();
    // str.truncate(err.len());
    // assert_eq!(str, err);
    assert!(res.is_ok());
    let res = res.unwrap();
    assert_eq!(res.output_stream_queue.len(), 1);
    assert_eq!(res.output_stream_queue[0].get_name(), "My ASN");
    assert_eq!(res.output_stream_queue[0].get_topic(), "My Asn was Seen!");
    assert_eq!(res.output_stream_queue[0].get_record().to_string(),
        "{\n\tasn: AS65534, \n\tmessage: 🤭 I, the messager, saw AS3200 in a BGP update.\n   }"
    );
}

#[test]
fn test_filter_map_message_01() {
    common::init();

    let code_line = src_code(
        r#"String.format("🤭 I, the messager, saw {} in a BGP update.", 3200)"#,
    );

    let res = test_data(
        Scope::Filter("my-message-filter-map-5".into()),
        &code_line,
    );

    trace!("{:#?}", res);
    // let err = "Eval error: Record {message: String, my_asn: Asn, } cannot"
    //     .to_string();
    // let mut str = res.unwrap_err().to_string();
    // str.truncate(err.len());
    // assert_eq!(str, err);
    assert!(res.is_ok());
    let res = res.unwrap();
    assert_eq!(res.output_stream_queue.len(), 1);
    assert_eq!(res.output_stream_queue[0].get_name(), "My ASN");
    assert_eq!(res.output_stream_queue[0].get_topic(), "My Asn was Seen!");
    assert_eq!(res.output_stream_queue[0].get_record().to_string(),
        "{\n\tasn: AS65534, \n\tmessage: 🤭 I, the messager, saw 3200 in a BGP update.\n   }"
    );
}

#[test]
fn test_filter_map_message_02() {
    common::init();

    let code_line = src_code(
        r#"String.format("🤭 I, the messager, saw {} in a BGP update.", route.next-hop)"#,
    );

    let res = test_data(
        Scope::Filter("my-message-filter-map-5".into()),
        &code_line,
    );

    trace!("{:#?}", res);
    // let err = "Eval error: Record {message: String, my_asn: Asn, } cannot"
    //     .to_string();
    // let mut str = res.unwrap_err().to_string();
    // str.truncate(err.len());
    // assert_eq!(str, err);
    assert!(res.is_ok());
    let res = res.unwrap();
    assert_eq!(res.output_stream_queue.len(), 1);
    assert_eq!(res.output_stream_queue[0].get_name(), "My ASN");
    assert_eq!(res.output_stream_queue[0].get_topic(), "My Asn was Seen!");
    assert_eq!(res.output_stream_queue[0].get_record().to_string(),
        "{\n\tasn: AS65534, \n\tmessage: 🤭 I, the messager, saw 193.0.0.23 in a BGP update.\n   }"
    );
}

#[test]
fn test_filter_map_message_03() {
    common::init();

    let code_line = src_code(
        r#"String.format("🤭 I, the messager, saw {} in a BGP update.", route.as-path)"#,
    );

    let res = test_data(
        Scope::Filter("my-message-filter-map-5".into()),
        &code_line,
    );

    trace!("{:#?}", res);
    // let err = "Eval error: Record {message: String, my_asn: Asn, } cannot"
    //     .to_string();
    // let mut str = res.unwrap_err().to_string();
    // str.truncate(err.len());
    // assert_eq!(str, err);
    assert!(res.is_ok());
    let res = res.unwrap();
    assert_eq!(res.output_stream_queue.len(), 1);
    assert_eq!(res.output_stream_queue[0].get_name(), "My ASN");
    assert_eq!(res.output_stream_queue[0].get_topic(), "My Asn was Seen!");
    assert_eq!(res.output_stream_queue[0].get_record().to_string(),
        "{\n\tasn: AS65534, \n\tmessage: 🤭 I, the messager, saw AS65534 AS65335 in a BGP update.\n   }"
    );
}

#[test]
fn test_filter_map_message_04() {
    common::init();

    let code_line = src_code(
        r#"String.format("🤭 I, the messager, saw {} in a BGP update.", route.communities)"#,
    );

    let res = test_data(
        Scope::Filter("my-message-filter-map-5".into()),
        &code_line,
    );

    trace!("{:#?}", res);
    // let err = "Eval error: Record {message: String, my_asn: Asn, } cannot"
    //     .to_string();
    // let mut str = res.unwrap_err().to_string();
    // str.truncate(err.len());
    // assert_eq!(str, err);
    assert!(res.is_ok());
    let res = res.unwrap();
    assert_eq!(res.output_stream_queue.len(), 1);
    assert_eq!(res.output_stream_queue[0].get_name(), "My ASN");
    assert_eq!(res.output_stream_queue[0].get_topic(), "My Asn was Seen!");
    assert_eq!(res.output_stream_queue[0].get_record().to_string(),
        "{\n\tasn: AS65534, \n\tmessage: 🤭 I, the messager, saw [AS32524:3340, AS32524:3348] in a BGP update.\n   }"
    );
}

#[test]
fn test_filter_map_message_05() {
    common::init();

    let code_line = src_code(
        r#"String.format("🤭 I, the messager, saw {} in a BGP update.", route)"#,
    );

    let res = test_data(
        Scope::Filter("my-message-filter-map-5".into()),
        &code_line,
    );

    trace!("{:#?}", res);
    // let err = "Eval error: Record {message: String, my_asn: Asn, } cannot"
    //     .to_string();
    // let mut str = res.unwrap_err().to_string();
    // str.truncate(err.len());
    // assert_eq!(str, err);
    assert!(res.is_ok());
    let res = res.unwrap();
    assert_eq!(res.output_stream_queue.len(), 1);
    assert_eq!(res.output_stream_queue[0].get_name(), "My ASN");
    assert_eq!(res.output_stream_queue[0].get_topic(), "My Asn was Seen!");
    assert_eq!(res.output_stream_queue[0].get_record().to_string(),
        "{\n\tasn: AS65534, \n\tmessage: 🤭 I, the messager, saw {\n\tas-path: AS65534 AS65335, \n\tbmp_msg_type: 1, \n\tcommunities: [AS32524:3340, AS32524:3348], \n\tlocal-pref: 20, \n\tmed: 80, \n\tnext-hop: 193.0.0.23, \n\torigin: AS211321, \n\tprefix: 193.0.0.0/24\n   } in a BGP update.\n   }"
    );
}
