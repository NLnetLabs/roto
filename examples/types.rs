use roto::types::{
    AsPath, Asn, Community, IpAddress, List, Prefix, RotoPrimitiveType,
    Record, RotoType, U32, U8,
};

fn main() {
    // let count = RotoType::Primitive(RotoPrimitiveType::U32(U32::new(1)));
    // let count2 = RotoType::Primitive(RotoPrimitiveType::Prefix(Prefix::new(
    //     routecore::addr::Prefix::new("193.0.0.0".parse().unwrap(), 24)
    //         .unwrap(),
    // )));
    // let ip_address =
    //     RotoType::Primitive(RotoPrimitiveType::IpAddress(IpAddress::new(
    //         std::net::IpAddr::V4(std::net::Ipv4Addr::new(193, 0, 0, 23)),
    //     )));

    // let asn = RotoType::Primitive(RotoPrimitiveType::Asn(Asn::new(
    //     routecore::asn::Asn::from_u32(1),
    // )));

    // let as_path =
    //     RotoType::Primitive(RotoPrimitiveType::AsPath(AsPath::new(vec![
    //         Asn::new(routecore::asn::Asn::from_u32(1)),
    //     ])));

    // let some_record = RotoType::Record(
    //     Record::new(vec![
    //         ("count", Box::new(count)),
    //         ("count2", &count2),
    //         ("ip_address", &ip_address),
    //         ("asn", &asn),
    //         ("as_path", &as_path),
    //         ("", &RotoType::Empty),
    //         ("", &RotoType::Empty),
    //     ])
    //     .unwrap(),
    // );

    // let comms = RotoType::List(List::new(vec![RotoType::Primitive(
    //     RotoPrimitiveType::Community(Community::Normal),
    // )]));

    let my_rec: Result<Record<6>, std::boxed::Box<_>> =
        RotoType::define(vec![
            ("count", "u32"),
            ("count2", "u8"),
            ("comms", "community"),
            ("ip_address", "ip_address"),
            ("asn", "asn"),
            ("as_path", "as_path"),
        ]);

    println!("{:?}", my_rec);
    println!("{:?}", my_rec.unwrap().get_value_by_field("as_path"));
}
