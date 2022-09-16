use roto::types::{
    AsPath, Asn, Community, CommunityType, ElementTypeValue, List, Prefix,
    PrimitiveTypeValue, Record, TypeDef, TypeValue, U32,
};

fn main() {
    // let count = RotoType::create_primitive_var(
    //     RotoType::Asn,
    //     RotoPrimitiveType::Asn(Asn::from_u32(1)),
    // )
    // .unwrap();

    let count =
        PrimitiveTypeValue::create_instance(TypeDef::U32, 1_u32).unwrap();

    let count2 = PrimitiveTypeValue::create_instance(
        TypeDef::Prefix,
        routecore::addr::Prefix::new("193.0.0.0".parse().unwrap(), 24)
            .unwrap(),
    ).unwrap();

    let ip_address = PrimitiveTypeValue::create_instance(
        TypeDef::IpAddress,
        std::net::IpAddr::V4(std::net::Ipv4Addr::new(193, 0, 0, 23)),
    )
    .unwrap();

    let as_path = PrimitiveTypeValue::create_instance(
        TypeDef::AsPath,
        PrimitiveTypeValue::AsPath(AsPath::new(vec![Asn::from_u32(1)])),
    )
    .unwrap();

    let asn = PrimitiveTypeValue::create_instance(
        TypeDef::Asn,
        Asn::from_u32(211321),
    )
    .unwrap();
    println!("{:?}", asn);

    let comms =
        TypeValue::List(List::new(vec![ElementTypeValue::Primitive(
            PrimitiveTypeValue::Community(Community::new(
                CommunityType::Normal,
            )),
        )]));

    let my_comms_type =
        TypeDef::List(Box::new(TypeDef::List(Box::new(TypeDef::Community))));

    let my_nested_rec_type =
        TypeDef::new_record_type(vec![("counter", TypeDef::U32)]).unwrap();

    let my_nested_rec_instance = Record::create_instance(
        &my_nested_rec_type,
        vec![(
            "counter",
            TypeValue::Primitive(PrimitiveTypeValue::U32(U32::new(1))),
        )],
    )
    .unwrap();

    let my_rec_type = TypeDef::new_record_type(vec![
        ("count", TypeDef::U32),
        ("count2", TypeDef::Prefix),
        ("ip_address", TypeDef::IpAddress),
        ("asn", TypeDef::Asn),
        ("as_path", TypeDef::AsPath),
        ("communities", my_comms_type),
        ("record", my_nested_rec_type),
    ])
    .unwrap();

    let my_record = Record::create_instance(
        &my_rec_type,
        vec![
            ("count", count),
            ("count2", count2),
            ("ip_address", ip_address),
            ("asn", asn),
            ("as_path", as_path),
            ("communities", comms),
            ("record", TypeValue::Record(my_nested_rec_instance)),
        ],
    )
    .unwrap();

    println!("{:?}", my_record);
    println!("{:?}", my_record.get_value_for_field("as_path"));
}
