#[cfg(test)]
mod route {
    use routecore::bgp::{
        message::SessionConfig,
        types::{NextHop, OriginType},
    };

    use crate::{
        types::builtin::{
            Asn, Prefix, RawRouteWithDeltas, RotondaId, RouteStatus,
            UpdateMessage,
        },
        vm::VmError,
    };

    #[test]
    fn create_update_msg() -> Result<(), VmError> {
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

        let update: UpdateMessage =
            UpdateMessage::new(buf, SessionConfig::modern());

        let prefixes: Vec<Prefix> = update
            .0
            .nlris()
            .iter()
            .filter_map(|n| n.prefix().map(|p| p.into()))
            .collect();
        let msg_id = (RotondaId(0), 0);

        let mut roto_msgs = vec![];

        roto_msgs.push(RawRouteWithDeltas::new_with_message(
            msg_id,
            prefixes[0],
            update,
            RouteStatus::InConvergence,
        ));

        for prefix in &prefixes[1..] {
            roto_msgs.push(RawRouteWithDeltas::new_with_message_ref(
                msg_id,
                *prefix,
                &roto_msgs[0].raw_message,
                RouteStatus::InConvergence,
            ))
        }

        println!("{:?}", roto_msgs);
        println!("{:#?}", roto_msgs[2].get_latest_attrs());

        assert_eq!(roto_msgs[0].prefix, prefixes[0]);
        assert_eq!(roto_msgs[1].prefix, prefixes[1]);
        assert_eq!(roto_msgs[2].prefix, prefixes[2]);
        assert_eq!(roto_msgs[3].prefix, prefixes[3]);
        assert_eq!(roto_msgs[4].prefix, prefixes[4]);
        assert_eq!(roto_msgs.len(), 5);

        let delta_id = (RotondaId(0), 1);

        let mut delta = roto_msgs[0].open_new_delta(delta_id)?;
        if let std::net::IpAddr::V6(v6) = prefixes[0].0.addr() {
            delta.attributes.next_hop.set(NextHop::Ipv6(v6));
        }

        let res = delta
            .attributes
            .as_path
            .prepend(crate::types::builtin::primitives::Asn::from(211321_u32));
        assert!(res.is_ok());

        let res = delta.attributes.origin_type.set(OriginType::Incomplete);
        assert_eq!(res, Some(OriginType::Igp.into()));

        println!("change set {:?}", delta);
        println!("as_path {:?}", &delta.attributes.as_path);

        let res = roto_msgs[2].store_delta(delta);
        assert!(res.is_ok());

        println!("materialize! {:#?}", roto_msgs[2].get_latest_attrs());

        let attr_set = roto_msgs[2].get_latest_attrs();
        assert_eq!(attr_set.as_path.len(), Some(2));

        println!(
            "ATTR_SET_AS_PATH {:?}",
            attr_set.as_path.as_routecore_hops_vec()
        );
        assert_eq!(
            attr_set.as_path.as_routecore_hops_vec().get(0),
            Asn::try_from(211321_u32)
                .ok()
                .map(move |a| routecore::bgp::aspath::Hop::from(a.0))
                .as_ref()
                .as_ref()
        );
        assert_eq!(
            attr_set.as_path.as_routecore_hops_vec().get(1),
            Asn::try_from(200_u32)
                .ok()
                .map(move |a| routecore::bgp::aspath::Hop::from(a.0))
                .as_ref()
                .as_ref()
        );
        Ok(())
    }

    #[test]
    fn create_layered_update_msg() -> Result<(), VmError> {
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

        let update: UpdateMessage =
            UpdateMessage::new(buf, SessionConfig::modern());

        let prefixes: Vec<Prefix> = update
            .0
            .nlris()
            .iter()
            .filter_map(|n| n.prefix().map(|p| p.into()))
            .collect();
        let msg_id = (RotondaId(0), 0);

        let mut roto_msgs = vec![];

        roto_msgs.push(RawRouteWithDeltas::new_with_message(
            msg_id,
            prefixes[0],
            update,
            RouteStatus::InConvergence,
        ));

        for prefix in &prefixes[1..] {
            roto_msgs.push(RawRouteWithDeltas::new_with_message_ref(
                msg_id,
                *prefix,
                &roto_msgs[0].raw_message,
                RouteStatus::InConvergence,
            ))
        }

        assert_eq!(roto_msgs[0].prefix, prefixes[0]);
        assert_eq!(roto_msgs[1].prefix, prefixes[1]);
        assert_eq!(roto_msgs[2].prefix, prefixes[2]);
        assert_eq!(roto_msgs[3].prefix, prefixes[3]);
        assert_eq!(roto_msgs[4].prefix, prefixes[4]);
        assert_eq!(roto_msgs.len(), 5);

        let delta_id = (RotondaId(0), 1);

        // Change Set 1
        let mut new_change_set1 = roto_msgs[2].open_new_delta(delta_id)?;

        println!("change set {:#?}", new_change_set1);
        if let std::net::IpAddr::V6(v6) = prefixes[2].0.addr() {
            new_change_set1.attributes.next_hop.set(NextHop::Ipv6(v6));
        }

        let res = new_change_set1.attributes.as_path.prepend(211321_u32); //].try_into().unwrap());
        assert!(res.is_ok());

        let res = new_change_set1
            .attributes
            .origin_type
            .set(OriginType::Incomplete);
        assert_eq!(res, Some(OriginType::Igp.into()));

        let res = roto_msgs[2].store_delta(new_change_set1);
        assert!(res.is_ok());

        let attr_set = roto_msgs[2].get_latest_attrs();
        assert_eq!(attr_set.as_path.len(), Some(2));
        assert_eq!(
            *attr_set.as_path.as_routecore_hops_vec()[0],
            routecore::bgp::aspath::Hop::from(
                Asn::try_from(211321_u32).unwrap().0
            )
        );
        assert_eq!(
            attr_set.as_path.as_routecore_hops_vec().get(1),
            Asn::try_from(200_u16)
                .ok()
                .map(move |a| routecore::bgp::aspath::Hop::from(a.0))
                .as_ref()
                .as_ref()
        );

        // Change Set 2
        let mut new_change_set2 = roto_msgs[2].open_new_delta(delta_id)?;
        let res = new_change_set2.attributes.as_path.prepend(211322_u32); //.try_into().unwrap());
        assert!(res.is_ok());

        let res = roto_msgs[2].store_delta(new_change_set2);
        assert!(res.is_ok());

        let attr_set = roto_msgs[2].get_latest_attrs();
        assert_eq!(attr_set.as_path.len(), Some(3));
        assert_eq!(
            attr_set.as_path.as_routecore_hops_vec().get(0),
            Asn::try_from(211322_u32)
                .ok()
                .map(move |a| routecore::bgp::aspath::Hop::from(a.0))
                .as_ref()
                .as_ref()
        );
        assert_eq!(
            attr_set.as_path.as_routecore_hops_vec().get(1),
            Asn::try_from(211321_u32)
                .ok()
                .map(move |a| routecore::bgp::aspath::Hop::from(a.0))
                .as_ref()
                .as_ref()
        );
        assert_eq!(
            attr_set.as_path.as_routecore_hops_vec().get(2),
            Asn::try_from(200_u32)
                .ok()
                .map(move |a| routecore::bgp::aspath::Hop::from(a.0))
                .as_ref()
                .as_ref()
        );
        println!("Before changeset3 {:#?}", &attr_set);

        // Change Set 3
        // let mut new_change_set3 = roto_msgs[2].open_new_delta(delta_id)?;
        // let res = new_change_set3.attributes.as_path.insert(1, vec![Asn::from(201)]);
        // assert!(res.is_ok());

        // let res = roto_msgs[2].store_delta(new_change_set3);
        // assert!(res.is_ok());

        // let attr_set = roto_msgs[2].get_latest_attrs();

        // println!("After changeset3 {:#?}", attr_set);

        // assert_eq!(attr_set.as_path.len(), Some(4));
        // assert_eq!(attr_set.as_path.get_from_vec(0).map(|s| s[0]), Asn::from(211322).into());
        // assert_eq!(attr_set.as_path.get_from_vec(0).map(|s| s[1]), Asn::from(201).into());
        // assert_eq!(attr_set.as_path.get_from_vec(0).map(|s| s[2]), Asn::from(211321).into());
        // assert_eq!(attr_set.as_path.get_from_vec(0).map(|s| s[3]), Asn::from(200).into());

        Ok(())
    }
}
