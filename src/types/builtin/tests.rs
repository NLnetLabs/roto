#[cfg(test)]
mod route {
    use routecore::{
        bgp::{
            message::{Message, SessionConfig, UpdateMessage},
            types::{OriginType, NextHop},
        }, asn::Asn,
    };

    use crate::types::builtin::{RawRouteWithDeltas, RotondaId};

    #[test]
    fn create_update_msg() {
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

        let update: UpdateMessage<_> =
            Message::from_octets(buf, Some(SessionConfig::modern()))
                .unwrap()
                .try_into()
                .unwrap();

        let prefixes: Vec<routecore::addr::Prefix> =
            update.nlris().iter().filter_map(|n| n.prefix()).collect();
        let msg_id = (RotondaId(0), 0);

        let mut roto_msgs = vec![];

        roto_msgs.push(RawRouteWithDeltas::new_with_message(
            msg_id,
            prefixes[0],
            update,
        ));

        for prefix in &prefixes[1..] {
            roto_msgs.push(RawRouteWithDeltas::new_with_message_ref(
                msg_id,
                *prefix,
                &roto_msgs[0].raw_message,
            ))
        }

        println!("{:?}", roto_msgs);
        println!(
            "{:#?}",
            roto_msgs[2].clone_latest_attrs()
        );

        assert_eq!(roto_msgs[0].prefix, prefixes[0]);
        assert_eq!(roto_msgs[1].prefix, prefixes[1]);
        assert_eq!(roto_msgs[2].prefix, prefixes[2]);
        assert_eq!(roto_msgs[3].prefix, prefixes[3]);
        assert_eq!(roto_msgs[4].prefix, prefixes[4]);
        assert_eq!(roto_msgs.len(), 5);

        let delta_id = (RotondaId(0), 1);

        let mut change_set = roto_msgs[0].clone_latest_attrs();
        if let std::net::IpAddr::V6(v6) = prefixes[0].addr() {
            change_set
                .next_hop.set(NextHop::Ipv6(v6));
        }

        let res = change_set.as_path.prepend(Asn::from(211321));
        assert!(res.is_ok());

        let res = change_set.origin_type.set(OriginType::Incomplete);
        assert_eq!(res, Some(OriginType::Igp));

        println!("change set {:?}", change_set);
        println!("as_path {:?}", &change_set.as_path);

        roto_msgs[2].add_delta(delta_id, change_set);

        println!(
            "materialize! {:#?}",
            roto_msgs[2].clone_latest_attrs()
        );

        let attr_set = roto_msgs[2].get_latest_attrs();
        assert_eq!(attr_set.as_path.len(), Some(2));
        assert_eq!(attr_set.as_path.get(0).unwrap(), &Asn::from(211321).into());
        assert_eq!(attr_set.as_path.get(1).unwrap(), &Asn::from(200).into());
    }
}
