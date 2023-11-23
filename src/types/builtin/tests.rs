#![cfg(test)]

use super::{PrefixLength, U16, U8, IntegerLiteral, AsPath, Asn};
use crate::{
    compiler::CompileError,
    traits::RotoType,
    types::builtin::U32,
};
use crate::types::builtin::BuiltinTypeValue;
use crate::types::typedef::TypeDef;

#[cfg(test)]
mod route {
    use routecore::bgp::{
        message::{
            nlri::{BasicNlri, Nlri},
            SessionConfig,
        },
        types::{NextHop, OriginType},
    };

    use crate::{
        types::
            builtin::{
                Asn, Prefix, RawRouteWithDeltas, RotondaId,
                RouteStatus, UpdateMessage,
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
            UpdateMessage::new(buf, SessionConfig::modern()).unwrap();

        let prefixes: Vec<Prefix> = update
            .0
            .announcements()
            .into_iter()
            .flat_map(|n| {
                n.filter_map(|p| {
                    if let Ok(Nlri::Unicast(BasicNlri { prefix, .. })) = p {
                        Some(Prefix::from(prefix))
                    } else {
                        None
                    }
                })
            })
            .collect();
        let msg_id = (RotondaId(0), 0);

        let mut roto_msgs = vec![];

        roto_msgs.push(RawRouteWithDeltas::new_with_message(
            msg_id,
            prefixes[0],
            update,
            RouteStatus::InConvergence,
        )?);

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
            delta
                .attributes
                .next_hop
                .set(NextHop::Unicast(std::net::IpAddr::V6(v6)));
        }

        let res = delta.attributes.as_path.prepend(
            crate::types::builtin::primitives::Asn::from(211321_u32),
        );
        assert!(res.is_ok());

        let res = delta.attributes.origin_type.set(OriginType::Incomplete);
        assert_eq!(res, Some(OriginType::Igp.into()));

        println!("change set {:?}", delta);
        println!("as_path {:?}", &delta.attributes.as_path);

        let res = roto_msgs[2].store_delta(delta);
        assert!(res.is_ok());

        println!("materialize! {:#?}", roto_msgs[2].get_latest_attrs());

        let attr_set = roto_msgs[2].get_latest_attrs()?;
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
            UpdateMessage::new(buf, SessionConfig::modern()).unwrap();

        let prefixes: Vec<Prefix> = update
            .0
            .announcements()
            .into_iter()
            .flat_map(|n| {
                n.filter_map(|p| {
                    if let Ok(Nlri::Unicast(BasicNlri { prefix, .. })) = p {
                        Some(Prefix::from(prefix))
                    } else {
                        None
                    }
                })
            })
            .collect();

        let msg_id = (RotondaId(0), 0);

        let mut roto_msgs = vec![];

        roto_msgs.push(RawRouteWithDeltas::new_with_message(
            msg_id,
            prefixes[0],
            update,
            RouteStatus::InConvergence,
        )?);

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
            new_change_set1
                .attributes
                .next_hop
                .set(NextHop::Unicast(std::net::IpAddr::V6(v6)));
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

        let attr_set = roto_msgs[2].get_latest_attrs()?;
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

        let attr_set = roto_msgs[2].get_latest_attrs()?;
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
        // let res = new_change_set3.attributes.as_path.insert(1, AsPath::from(vec![Asn::from(201)]));
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

#[cfg(test)]
fn test_method_on_type_value<RT: RotoType + Clone, TT: RotoType + Clone> (
    from_value: RT,
    method_name: &str,
    arg_value: TT,
) -> Result<(), CompileError> where BuiltinTypeValue: From<RT>, BuiltinTypeValue: From<TT> {

    // The type definition of the from value
    let src_ty: TypeDef = <BuiltinTypeValue>::from(from_value.clone()).into();
    let m = RT::get_props_for_method(
        TypeDef::Unknown,
        &crate::ast::Identifier {
            ident: method_name.into(),
        },
    )?;

    // Establish the type of the argument value.
    let arg_ty: TypeDef = <BuiltinTypeValue>::from(arg_value.clone()).into();

    // Test the evaluation conversion (as defined in the typedefconversion
    // macro in typedef.rs).
    assert!(src_ty.clone().test_type_conversion(arg_ty));

    // Test the compilation refinement type conversion.
    let arg_value = arg_value.clone().into_type(&src_ty)?;

    let set_op = from_value
        .exec_consume_value_method(
            m.method_token.try_into()?,
            vec![arg_value.clone()],
            src_ty,
        )
        .unwrap();
    assert_eq!(set_op, arg_value);

    Ok(())
}

#[cfg(test)]
fn mk_converted_type_value<RT: RotoType, TT: RotoType + Clone>(
    from_value: RT,
    // new_type: TypeDef,
    to_value: TT,
) -> Result<(), CompileError> where BuiltinTypeValue: From<TT> {

    let to_ty: TypeDef = <BuiltinTypeValue>::from(to_value.clone()).into();

    let m = from_value.into_type(&to_ty)?;
    assert_eq!(m, to_value.into());
    Ok(())
}

// ----------- Test: U8 ------------------------------------------------------

// From U8(StringLiteral,U16,U32,PrefixLength,IntegerLiteral;),
#[test]
fn test_u8() -> Result<(), CompileError> {
    let test_value = U8::new(0_u8);
    let res = U8::new(127);

    test_method_on_type_value(test_value, "set", res)
}

#[test]
fn test_u8_to_u16() -> Result<(), CompileError> {
    let test_value = U8::new(0_u8);
    let res = U16::new(127);

    test_method_on_type_value(test_value, "set", res)
}

#[test]
#[should_panic = "src_ty.clone().test_type_conversion(arg_ty)"]
fn test_invalid_u8_to_as_path() {
    let test_value = U8::new(0_u8);
    let arg = AsPath::new(vec![24.into()]).unwrap();

    test_method_on_type_value(test_value, "set", arg).unwrap();
}

#[test]
fn test_u8_conversion_prefix_length() -> Result<(), CompileError> {
    let test_value = U8::new(24_u8);
    let res = PrefixLength::new(24);

    mk_converted_type_value(test_value, res)
}

#[test]
#[should_panic = "Prefix length must be between 0 and 128, not 255"]
fn test_u8_conversion_invalid_prefix_length() {
    let test_value = U8::new(255_u8);
    let res = PrefixLength::new(255_u8);

    mk_converted_type_value(test_value, res).unwrap();
}

#[test]
fn test_u8_conversion_u16() -> Result<(), CompileError> {
    let test_value = U8::new(24_u8);
    let res = U16::new(24);

    mk_converted_type_value(test_value, res)
}

#[test]
fn test_conversion_integer_literal_u8() -> Result<(), CompileError> {
    let res = U8::new(24_u8);
    let test_value = IntegerLiteral::new(24);

    mk_converted_type_value(test_value, res)
}


#[test]
#[should_panic = "Cannot convert type U8 to type AsPath"]
fn test_u8_conversion_as_path() {
    let test_value = U8::new(24_u8);
    let res = AsPath::new(vec![24.into()]).unwrap();

    mk_converted_type_value(test_value, res).unwrap();
}

//------------ Test: U16 -----------------------------------------------------

#[test]
fn test_u16() -> Result<(), CompileError> {
    let test_value = U16::new(0_u16);
    let res = U16::new(127);

    test_method_on_type_value(test_value, "set", res)
}

#[test]
#[should_panic = "Cannot convert type U32 into type U16"]
fn test_invalid_u16() {
    let test_value = U16::new(0_u16);
    let res = U32::new(127);

    test_method_on_type_value(test_value, "set", res).unwrap();
}

#[test]
fn test_u16_conversion_u32() -> Result<(), CompileError> {
    let test_value = U16::new(127_u16);
    let res = U32::new(127);

    mk_converted_type_value(test_value, res)
}

#[test]
fn test_u16_conversion_prefix_length() -> Result<(), CompileError> {
    let test_value = U16::new(24_u16);
    let res = PrefixLength::new(24);

    mk_converted_type_value(test_value, res)
}

#[test]
#[should_panic = "Cannot convert an instance of type U16 with a value greater \
than 128 into type PrefixLength"]
fn test_u16_conversion_invalid_prefix_length() {
    let test_value = U16::new(255_u16);
    let res = PrefixLength::new(255);

    mk_converted_type_value(test_value, res).unwrap();
}

#[test]
fn test_conversion_integer_literal_u16() -> Result<(), CompileError> {
    let test_value = IntegerLiteral::new(32768);
    let res = U16::new(32768_u16);

    mk_converted_type_value(test_value, res)
}

//------------ Test: U32 -----------------------------------------------------

#[test]
fn test_u32() -> Result<(), CompileError> {
    let test_value = U32::new(2377_u32);
    let res = U32::new(12708786);

    test_method_on_type_value(test_value, "set", res)
}

#[test]
fn test_invalid_u32() {
    let test_value = U32::new(710_u32);
    let res = Asn::from(710_u32);

    test_method_on_type_value(test_value, "set", res).unwrap();
}

#[test]
fn test_u32_conversion_prefix_length() -> Result<(), CompileError> {
    let test_value = U32::new(24_u32);
    let res = PrefixLength::new(24);

    mk_converted_type_value(test_value, res)
}

#[test]
#[should_panic = "Cannot convert an instance of type U32 with a value \
greater than 128 into type PrefixLength"]
fn test_u32_conversion_invalid_prefix_length() {
    let test_value = U32::new(122255_u32);
    let res = PrefixLength::new(255);

    mk_converted_type_value(test_value, res).unwrap();
}

#[test]
fn test_conversion_integer_literal_u32() -> Result<(), CompileError> {
    let res = U32::new(32768_u32);
    let test_value = IntegerLiteral::new(32768);

    mk_converted_type_value(test_value, res)
}

#[test]
fn test_conversion_asn_u32() -> Result<(), CompileError> {
    let test_value = U32::new(32768_u32);
    let res = Asn::new(32768.into());

    mk_converted_type_value(test_value, res)
}