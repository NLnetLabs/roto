use std::ops::Deref;

use roto::types::{Record, RotoType, TypeName};

fn main() {
    // let count = RotoType::Primitive(RotoPrimitiveType::U32(U32::new(1)));
    // let count2 = RotoType::Primiti&ve(RotoPrimitiveType::Prefix(Prefix::new(
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

    let my_comms = TypeName::List(Box::new(TypeName::Community));

    let my_record_kvs = vec![
        ("count", &TypeName::U32),
        ("count2", &TypeName::Prefix),
        ("ip_address", &TypeName::IpAddress),
        ("asn", &TypeName::Asn),
        ("as_path", &TypeName::AsPath),
        ("communities", &my_comms),
    ];

    let my_rec: Record<'_, 6> =
        RotoType::define(my_record_kvs).unwrap();

    println!("{:?}", my_rec);
    println!("{:?}", my_rec.get_value_for_field("as_path"));
}
