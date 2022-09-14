use roto::types::{
    AsPath, Asn, Community, CommunityType, ElementType, IpAddress, List,
    Prefix, RotoPrimitiveType, RotoType, TypeName, U32,
};

fn main() {
    let count = RotoType::Primitive(RotoPrimitiveType::U32(U32::new(1)));
    let count2 = RotoType::Primitive(RotoPrimitiveType::Prefix(Prefix::new(
        routecore::addr::Prefix::new("193.0.0.0".parse().unwrap(), 24)
            .unwrap(),
    )));
    let ip_address =
        RotoType::Primitive(RotoPrimitiveType::IpAddress(IpAddress::new(
            std::net::IpAddr::V4(std::net::Ipv4Addr::new(193, 0, 0, 23)),
        )));

    let asn = RotoType::Primitive(RotoPrimitiveType::Asn(Asn::new(
        routecore::asn::Asn::from_u32(1),
    )));
    println!("{:?}", asn);
    let as_path =
        RotoType::Primitive(RotoPrimitiveType::AsPath(AsPath::new(vec![
            Asn::new(routecore::asn::Asn::from_u32(1)),
        ])));

    let comms = RotoType::List(List::new(vec![ElementType::Primitive(
        RotoPrimitiveType::Community(Community::new(CommunityType::Normal)),
    )]));

    let my_comms_type = TypeName::List(Box::new(TypeName::List(Box::new(
        TypeName::Community,
    ))));

    let my_rec_type = RotoType::new_record_type(vec![
        ("count", TypeName::U32),
        ("count2", TypeName::Prefix),
        ("ip_address", TypeName::IpAddress),
        ("asn", TypeName::Asn),
        ("as_path", TypeName::AsPath),
        ("communities", my_comms_type),
    ])
    .unwrap();

    let my_record = my_rec_type
        .new_record(vec![
            ("count", count),
            ("count2", count2),
            ("ip_address", ip_address),
            ("asn", asn),
            ("as_path", as_path),
            ("communities", comms),
        ])
        .unwrap();

    println!("{:?}", my_record);
    println!("{:?}", my_record.get_value_for_field("as_path"));
}
