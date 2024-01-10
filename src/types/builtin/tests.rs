#[cfg(test)]
mod route {
    use std::net::IpAddr;

    use super::super::{
        IntegerLiteral, PrefixLength, StringLiteral,
    };
    use crate::ast::{IpAddressLiteral, AsnLiteral};
    use crate::types::builtin::BuiltinTypeValue;
    use crate::types::typedef::TypeDef;
    use crate::types::typevalue::TypeValue;
    use crate::{
        compiler::CompileError, traits::RotoType,
    };

    enum MethodType {
        Value,
        Type,
        _Consume,
    }
    use routecore::addr::Prefix;
    use routecore::bgp::aspath::HopPath;
    use routecore::asn::{Asn, Asn16};
    use routecore::bgp::communities::{HumanReadableCommunity as Community, StandardCommunity, Tag, ExtendedCommunity, LargeCommunity};
    use routecore::bgp::types::LocalPref;
    use routecore::bgp::{
        message::{
            nlri::{BasicNlri, Nlri},
            SessionConfig,
        },
        types::{NextHop, OriginType},
    };

    use std::str::FromStr;

    use crate::{
        types::builtin::{
            RawRouteWithDeltas, RotondaId, RouteStatus, UpdateMessage,
        },
        vm::VmError,
    };

    use std::io::Write;

    pub fn init() {
        let _ = env_logger::builder()
            .format(|buf, record| writeln!(buf, "{}", record.args()))
            .is_test(true)
            .try_init();
    }

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

        let prefixes: Vec<routecore::addr::Prefix> = update
            .0
            .announcements()
            .into_iter()
            .flat_map(|n| {
                n.filter_map(|p| {
                    if let Ok(Nlri::Unicast(BasicNlri { prefix, .. })) = p {
                        Some(prefix)
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
            routecore::bgp::types::AfiSafi::Ipv6Unicast,
            None,
            RouteStatus::InConvergence,
        )?);

        for prefix in &prefixes[1..] {
            roto_msgs.push(RawRouteWithDeltas::new_with_message_ref(
                msg_id,
                *prefix,
                &roto_msgs[0].raw_message,
                routecore::bgp::types::AfiSafi::Ipv6Unicast,
                None,
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
        if let std::net::IpAddr::V6(v6) = prefixes[0].addr() {
            delta
                .attributes
                .next_hop
                .set(NextHop::Unicast(std::net::IpAddr::V6(v6)));
        }

        let res = delta.attributes.as_path.prepend(
            Asn::from(211321_u32),
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
            *attr_set.as_path.as_routecore_hops_vec().first().unwrap(),
            &routecore::bgp::aspath::Hop::from(Asn::from(211321_u32))
        );
        assert_eq!(
            *attr_set.as_path.as_routecore_hops_vec().get(1).unwrap(),
            &routecore::bgp::aspath::Hop::from(Asn::from(200_u32))
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

        let prefixes: Vec<routecore::addr::Prefix> = update
            .0
            .announcements()
            .into_iter()
            .flat_map(|n| {
                n.filter_map(|p| {
                    if let Ok(Nlri::Unicast(BasicNlri { prefix, .. })) = p {
                        Some(prefix)
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
            routecore::bgp::types::AfiSafi::Ipv6Unicast,
            None,
            RouteStatus::InConvergence,
        )?);

        for prefix in &prefixes[1..] {
            roto_msgs.push(RawRouteWithDeltas::new_with_message_ref(
                msg_id,
                *prefix,
                &roto_msgs[0].raw_message,
                routecore::bgp::types::AfiSafi::Ipv6Unicast,
                None,
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
        if let std::net::IpAddr::V6(v6) = prefixes[2].addr() {
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
                Asn::from(211321_u32)
            )
        );
        assert_eq!(
            *attr_set.as_path.as_routecore_hops_vec().get(1).unwrap(),
            &routecore::bgp::aspath::Hop::from(Asn::from(200_u32))
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
            *attr_set.as_path.as_routecore_hops_vec().first().unwrap(),
            &routecore::bgp::aspath::Hop::from(Asn::from(211322_u32))
        );
        assert_eq!(
            *attr_set.as_path.as_routecore_hops_vec().get(1).unwrap(),
            &routecore::bgp::aspath::Hop::from(Asn::from(211321_u32))
        );
        assert_eq!(
            *attr_set.as_path.as_routecore_hops_vec().get(2).unwrap(),
            &routecore::bgp::aspath::Hop::from(routecore::asn::Asn::from(200_u32))
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

    // This tests operates in the same order as the Roto Compiler, i.e. it first
    // extracts the typedef from the TypeValue,then tests a possible
    // typedefconversion (with the typedefconversion macro), and finally performs
    // the actual conversion. The typedefconversion macro is more lenient than the
    // value into typedef, whereas the `into_type()` is more strict than the first
    // step. So testing typedefconversion and into_type() in isolation is good,
    // but isn't the complete picture.
    #[cfg(test)]
    fn test_consume_method_on_type_value<
        RT: RotoType + Clone,
        TT: RotoType + Clone,
    >(
        from_value: RT,
        method_name: &str,
        arg_value: TT,
    ) -> Result<(), CompileError>
    where
        BuiltinTypeValue: From<RT>,
        BuiltinTypeValue: From<TT>,
    {
        init();
        // The type definition of the from value
        let src_ty: TypeDef =
            <BuiltinTypeValue>::from(from_value.clone()).into();
        let m = RT::get_props_for_method(
            TypeDef::Unknown,
            &crate::ast::Identifier {
                ident: method_name.into(),
            },
        )?;

        // Establish the type of the argument value.
        let arg_ty: TypeDef =
            <BuiltinTypeValue>::from(arg_value.clone()).into();

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
    fn test_method_on_type_value_with_multiple_args<
        RT: RotoType + Clone,
        TT: RotoType + Clone,
        UT: RotoType + Clone,
    >(
        from_value: RT,
        method_type: MethodType,
        method_name: &str,
        arg_values: &[TT],
        expect: UT,
    ) -> Result<(), CompileError>
    where
        BuiltinTypeValue: From<RT>,
        BuiltinTypeValue: From<TT>,
    {
        // The type definition of the from value

        use crate::vm::StackValue;
        let src_ty: TypeDef =
            <BuiltinTypeValue>::from(from_value.clone()).into();
        let m = RT::get_props_for_method(
            TypeDef::Unknown,
            &crate::ast::Identifier {
                ident: method_name.into(),
            },
        )?;

        // Establish the type of the argument value.
        let validated_arg_values: Vec<TypeValue> = arg_values
            .iter()
            .map(|av| {
                let arg_ty = <BuiltinTypeValue>::from(av.clone()).into();
                assert!(src_ty.clone().test_type_conversion(arg_ty));
                av.clone().into_type(&src_ty)
            })
            .filter_map(|av| if let Ok(av) = av { Some(av) } else { None })
            .collect::<Vec<_>>();

        assert_eq!(arg_values.len(), validated_arg_values.len());

        // Test the evaluation conversion (as defined in the typedefconversion
        // macro in typedef.rs).

        // Test the compilation refinement type conversion.
        // let arg_value = arg_value.clone().into_type(&src_ty)?;

        let set_ops = match method_type {
            MethodType::_Consume => from_value
                .exec_consume_value_method(
                    m.method_token.try_into()?,
                    validated_arg_values,
                    src_ty,
                )
                .unwrap(),
            MethodType::Type => {
                let stack_values = validated_arg_values
                    .iter()
                    .map(StackValue::Ref)
                    .collect::<Vec<_>>();
                RT::exec_type_method(
                    m.method_token.try_into()?,
                    &stack_values,
                    src_ty,
                )
                .unwrap()
            }
            MethodType::Value => {
                let stack_values = validated_arg_values
                    .iter()
                    .map(StackValue::Ref)
                    .collect::<Vec<_>>();
                from_value
                    .exec_value_method(
                        m.method_token.try_into()?,
                        &stack_values,
                        src_ty,
                    )
                    .unwrap()
            }
        };

        assert_eq!(set_ops, expect.into());

        Ok(())
    }

    // This test only performs the value -> value conversion. The caveat of this
    // test is that errors may be returned from it, that will never appear in the
    // compiler chain, since these cases would already be thrown out by it before
    // it reaches the conversion. The VM also uses the `into_type()` method tested
    // here, and although the same caveats apply, there is one notable exception:
    // values that are being passed in at runtime (from external data sources).
    // These are out of control of the compiler and the VM so that then the `bare`
    // `into_type()` method as tested here applies.
    #[cfg(test)]
    fn mk_converted_type_value<RT: RotoType, TT: RotoType + Clone>(
        from_value: RT,
        // new_type: TypeDef,
        to_value: TT,
    ) -> Result<(), CompileError>
    where
        BuiltinTypeValue: From<TT>,
    {
        use log::trace;

        let to_ty: TypeDef =
            <BuiltinTypeValue>::from(to_value.clone()).into();

        trace!("from {:?} to {:?}", from_value, to_ty);
        let m = from_value.into_type(&to_ty)?;
        assert_eq!(m, to_value.into());
        Ok(())
    }

    // ----------- Test: U8 ------------------------------------------------------

    // From U8(StringLiteral,U16,U32,PrefixLength,IntegerLiteral;),
    #[test]
    fn test_u8() -> Result<(), CompileError> {
        init();
        let test_value = 0_u8;
        let res = 127_u8;

        test_consume_method_on_type_value(test_value, "set", res)
    }

    #[test]
    fn test_u8_to_string_literal() {
        let tv = BuiltinTypeValue::from(132_u8);
        assert_eq!(tv.into_type(&TypeDef::StringLiteral).unwrap(), TypeValue::from("132"));
    }

    #[test]
    fn test_u8_to_u16() -> Result<(), CompileError> {
        init();

        let test_value = 0_u8;
        let res = 127_u16;

        test_consume_method_on_type_value(test_value, "set", res)
    }

    #[test]
    fn test_u8_to_u32() -> Result<(), CompileError> {
        init();

        let test_value = 0_u8;
        let res = 127_u32;

        test_consume_method_on_type_value(test_value, "set", res)
    }

    #[test]
    #[should_panic = "src_ty.clone().test_type_conversion(arg_ty)"]
    fn test_invalid_u8_to_as_path() {
        init();

        let test_value = 0_u8;
        let arg: HopPath = vec![Asn::from(24)].into();

        test_consume_method_on_type_value(test_value, "set", arg).unwrap();
    }

    #[test]
    fn test_u8_conversion_prefix_length() -> Result<(), CompileError> {
        init();

        let test_value = 24_u8;
        let res = PrefixLength::new(24);

        mk_converted_type_value(test_value, res)
    }

    #[test]
    #[should_panic = "Prefix length must be between 0 and 128, not 255"]
    fn test_u8_conversion_invalid_prefix_length() {
        let test_value = 255_u8;
        let res = PrefixLength::new(255_u8);

        mk_converted_type_value(test_value, res).unwrap();
    }

    #[test]
    fn test_u8_conversion_u16() -> Result<(), CompileError> {
        let test_value = 24_u8;
        let res = 24_u16;

        mk_converted_type_value(test_value, res)
    }

    #[test]
    fn test_conversion_integer_literal_u8() -> Result<(), CompileError> {
        let res = 24_u8;
        let test_value = IntegerLiteral::new(24);

        mk_converted_type_value(test_value, res)
    }

    #[test]
    #[should_panic = "Cannot convert type U8 to type AsPath"]
    fn test_u8_conversion_as_path() {
        let test_value = 24_u8;
        let res: HopPath = vec![Asn::from(24)].into();

        mk_converted_type_value(test_value, res).unwrap();
    }

    //------------ Test: U16 -----------------------------------------------------

    #[test]
    fn test_u16() -> Result<(), CompileError> {
        let test_value = 0_u16;
        let res = 127_u16;

        test_consume_method_on_type_value(test_value, "set", res)
    }

    #[test]
    #[should_panic = "Cannot convert type U32 into type U16"]
    fn test_invalid_u16() {
        let test_value = 0_u16;
        let res = 127_u32;

        test_consume_method_on_type_value(test_value, "set", res).unwrap();
    }

    #[test]
    fn test_u16_conversion_u32() -> Result<(), CompileError> {
        let test_value = 127_u16;
        let res = 127_u32;

        mk_converted_type_value(test_value, res)
    }

    #[test]
    fn test_u16_conversion_prefix_length() -> Result<(), CompileError> {
        let test_value = 24_u16;
        let res = PrefixLength::new(24);

        mk_converted_type_value(test_value, res)
    }

    #[test]
    #[should_panic = "Cannot convert an instance of type U16 with a value greater \
than 128 into type PrefixLength"]
    fn test_u16_conversion_invalid_prefix_length() {
        let test_value = 255_u16;
        let res = PrefixLength::new(255_u8);

        mk_converted_type_value(test_value, res).unwrap();
    }

    #[test]
    fn test_conversion_integer_literal_u16() -> Result<(), CompileError> {
        let test_value = IntegerLiteral::new(32768);
        let res = 32768_u16;

        mk_converted_type_value(test_value, res)
    }

    #[test]
    fn test_conversion_asn_literal_u16() -> Result<(), CompileError> {
        let test_value = IntegerLiteral::new(32768);
        let res = Asn::from(32768_u32);

        mk_converted_type_value(test_value, res)
    }

    //------------ Test: U32 -----------------------------------------------------

    #[test]
    fn test_u32() -> Result<(), CompileError> {
        let test_value = 2377_u32;
        let res = 12708786_u32;

        test_consume_method_on_type_value(test_value, "set", res)
    }

    #[test]
    fn test_invalid_u32() {
        let test_value = 710_u32;
        let res = Asn::from(710_u32);

        test_consume_method_on_type_value(test_value, "set", res).unwrap();
    }

    #[test]
    fn test_u32_conversion_prefix_length() -> Result<(), CompileError> {
        let test_value = 24_u32;
        let res = PrefixLength::new(24);

        mk_converted_type_value(test_value, res)
    }

    #[test]
    #[should_panic = "Cannot convert an instance of type U32 with a value \
greater than 128 into type PrefixLength"]
    fn test_u32_conversion_invalid_prefix_length() {
        let test_value = 122255_u32;
        let res = PrefixLength::new(255);

        mk_converted_type_value(test_value, res).unwrap();
    }

    #[test]
    fn test_conversion_integer_literal_u32() -> Result<(), CompileError> {
        let res = 32768_u32;
        let test_value = IntegerLiteral::new(32768);

        mk_converted_type_value(test_value, res)
    }

    #[test]
    fn test_conversion_asn_u32() -> Result<(), CompileError> {
        let test_value = 32768_u32;
        let res = Asn::from(32768);

        mk_converted_type_value(test_value, res)
    }

    //------------ Test: Boolean -------------------------------------------------

    #[test]
    fn test_boolean() -> Result<(), CompileError> {
        let test_value = true;
        let res = false;

        test_consume_method_on_type_value(test_value, "set", res)
    }

    #[test]
    #[should_panic = "assertion failed: \
src_ty.clone().test_type_conversion(arg_ty)"]
    fn test_invalid_boolean() {
        let test_value = true;
        let res = Asn::from(710_u32);

        test_consume_method_on_type_value(test_value, "set", res).unwrap();
    }

    #[test]
    #[should_panic = "Unknown method: 'blaffer' for type Bool"]
    fn test_invalid_method_boolean() {
        let test_value = true;
        let res = Asn::from(710_u32);

        test_consume_method_on_type_value(test_value, "blaffer", res)
            .unwrap();
    }

    #[test]
    fn test_conversion_to_string() -> Result<(), CompileError> {
        let test_value = true;
        let res = StringLiteral::new("true".into());

        mk_converted_type_value(test_value, res)
    }

    #[test]
    fn test_conversion_from_string() -> Result<(), CompileError> {
        let test_value = StringLiteral::new("true".into());
        let res = true;

        mk_converted_type_value(test_value, res)
    }

    //------------ Test: StringLiteral -------------------------------------------

    #[test]
    fn test_string_set() -> Result<(), CompileError> {
        let test_value = StringLiteral::new("blaffer".into());
        let res = StringLiteral::new("blaffer".into());

        test_consume_method_on_type_value(test_value, "set", res)
    }

    #[test]
    fn test_string_format_1() -> Result<(), CompileError> {
        let test_value = StringLiteral::new("blaffer {}".into());
        let infix = StringLiteral::new("bluffer".into());
        let res = StringLiteral::new("blaffer bluffer".into());

        test_method_on_type_value_with_multiple_args(
            test_value.clone(),
            MethodType::Type,
            "format",
            &[test_value, infix],
            res,
        )
    }

    // this is kinda' weird, if the format method cannot find the `{}` symbol it
    // will just concatenate the replacement string to the end of the source
    // string.
    #[test]
    fn test_string_format_2() -> Result<(), CompileError> {
        let test_value = StringLiteral::new("blaffer".into());
        let infix = StringLiteral::new("bluffer".into());
        let res = StringLiteral::new("blafferbluffer".into());

        test_method_on_type_value_with_multiple_args(
            test_value.clone(),
            MethodType::Type,
            "format",
            &[test_value, infix],
            res,
        )
    }

    #[test]
    fn test_string_cmp_1() -> Result<(), CompileError> {
        let test_value_1 = StringLiteral::new("blaffer".into());
        let test_value_2 = StringLiteral::new("blaffer".into());

        test_method_on_type_value_with_multiple_args(
            test_value_1.clone(),
            MethodType::Type,
            "cmp",
            &[test_value_1, test_value_2],
            TypeValue::from(true),
        )
    }

    #[test]
    fn test_string_cmp_2() -> Result<(), CompileError> {
        let test_value_1 = StringLiteral::new("blaffer".into());
        let test_value_2 = StringLiteral::new("blyffer".into());

        test_method_on_type_value_with_multiple_args(
            test_value_1.clone(),
            MethodType::Type,
            "cmp",
            &[test_value_1, test_value_2],
            TypeValue::from(false),
        )
    }

    #[test]
    fn test_string_cmp_3() -> Result<(), CompileError> {
        let test_value_1 = StringLiteral::new("blaffer".into());
        let test_value_2 = StringLiteral::new("blaffer".into());

        test_method_on_type_value_with_multiple_args(
            test_value_1.clone(),
            MethodType::Value,
            "cmp",
            &[test_value_2],
            TypeValue::from(true),
        )
    }

    #[test]
    fn test_string_cmp_4() -> Result<(), CompileError> {
        let test_value_1 = StringLiteral::new("blaffer".into());
        let test_value_2 = StringLiteral::new("bloppper".into());

        test_method_on_type_value_with_multiple_args(
            test_value_1.clone(),
            MethodType::Value,
            "cmp",
            &[test_value_2],
            TypeValue::from(false),
        )
    }

    // We don't do crazy conversions from Boolean to StringLiteral in comparisons.
    #[test]
    #[should_panic = "assertion failed: \
src_ty.clone().test_type_conversion(arg_ty)"]
    fn test_string_cmp_5() {
        let test_value_1 = StringLiteral::new("blaffer".into());
        let test_value_2 = true;

        test_method_on_type_value_with_multiple_args(
            test_value_1.clone(),
            MethodType::Value,
            "cmp",
            &[test_value_2],
            TypeValue::from(false),
        )
        .unwrap();
    }

    #[test]
    fn test_string_cmp_6() -> Result<(), CompileError> {
        let test_value_1 = StringLiteral::new("blaffer".into());
        let test_value_2 = StringLiteral::new("bloppper".into());

        test_consume_method_on_type_value(
            test_value_1.clone(),
            "set",
            test_value_2,
        )
    }

    #[test]
    #[should_panic = r#"called `Result::unwrap()` on an `Err` value: User("Unknown method: '\n\nsd9876ujklkf;j;jgkh' for type StringLiteral")"#]
    fn test_string_cmp_7() {
        let test_value_1 = StringLiteral::new("blaffer".into());
        let test_value_2 = StringLiteral::new("bloppper".into());

        test_consume_method_on_type_value(
            test_value_1.clone(),
            "\n\nsd9876ujklkf;j;jgkh",
            test_value_2,
        )
        .unwrap();
    }

    //-------- Test: IntegerLiteral ------------------------------------------

    #[test]
    fn test_integer_literal_1() -> Result<(), CompileError> {
        let test_value = IntegerLiteral::new(100);
        let res = Asn::from(100);

        mk_converted_type_value(test_value, res)
    }

    #[test]
    #[should_panic = r#"called `Result::unwrap()` on an `Err` value: User("Cannot convert type IntegerLiteral > 4294967295 into Asn")"#]
    fn test_integer_literal_2() {
        let test_value = IntegerLiteral::new(1042949672950);
        test_value.into_type(&TypeDef::Asn).unwrap();
    }

    #[test]
    #[should_panic = r#"called `Result::unwrap()` on an `Err` value: User("Cannot convert type IntegerLiteral < 0 into Asn")"#]
    fn test_integer_literal_3() {
        let test_value = IntegerLiteral::new(-100);
        test_value.into_type(&TypeDef::Asn).unwrap();
    }

    #[test]
    fn test_integer_literal_4() {
        let test_value = IntegerLiteral::new(255);
        test_value.into_type(&TypeDef::U8).unwrap();
    }

    #[test]
    #[should_panic = r#"called `Result::unwrap()` on an `Err` value: User("Cannot convert instance of type IntegerLiteral with value 256 into U8")"#]
    fn test_integer_literal_5() {
        let test_value = IntegerLiteral::new(256);
        test_value.into_type(&TypeDef::U8).unwrap();
    }

    //-------- Test: StringLiteral -------------------------------------------

    #[test]
    fn test_prefix_1() {
        let test_value: Prefix = Prefix::new("10.1.1.0".parse().unwrap(), 24).unwrap();
        test_value.into_type(&TypeDef::StringLiteral).unwrap();
    }

    #[test]
    #[should_panic = r#"Result::unwrap()` on an `Err` value: User("Cannot convert type Prefix to type AsPath")"#]
    fn test_prefix_2() {
        let test_value: Prefix = Prefix::new("10.1.1.0".parse().unwrap(), 24).unwrap();
        test_value.into_type(&TypeDef::AsPath).unwrap();
    }

    //-------- Test: PrefixLength --------------------------------------------

    #[test]
    fn test_prefix_length_literal_1() {
        let test_value: PrefixLength = PrefixLength(23);
        let res = IntegerLiteral::new(23);

        mk_converted_type_value(test_value, res).unwrap();
    }

    #[test]
    fn test_prefix_length_literal_2() {
        let test_value: PrefixLength = PrefixLength(23);
        let res = 23_u8;

        mk_converted_type_value(test_value, res).unwrap();
    }

    #[test]
    fn test_prefix_length_literal_3() {
        let test_value: PrefixLength = PrefixLength(23);
        let res = 23_u16;

        mk_converted_type_value(test_value, res).unwrap();
    }

    #[test]
    fn test_prefix_length_literal_4() {
        let test_value: PrefixLength = PrefixLength(23);
        let res = 23_u32;

        mk_converted_type_value(test_value, res).unwrap();
    }

    #[test]
    fn test_prefix_length_literal_5() {
        let test_value: PrefixLength = PrefixLength(18);
        let res = IntegerLiteral::new(23);

        test_method_on_type_value_with_multiple_args(test_value, MethodType::Value, "set", &[23_u32], res).unwrap();
    }

    #[test]
    fn test_local_pref_1() {
        let test_value: LocalPref = LocalPref(100);
        let res = IntegerLiteral::new(100);

        mk_converted_type_value(test_value, res).unwrap();
    }

    #[test]
    fn test_local_pref_2() {
        let test_value: LocalPref = LocalPref(100);
        let res = 100_u32;

        mk_converted_type_value(test_value, res).unwrap();
    }

    //-------- Test: Communities ---------------------------------------------

    #[test]
    fn test_standard_community_1() {
        let test_value: Community = Community::from_str("AS1234:7890").unwrap();
        let res = StringLiteral::new("AS1234:7890".to_string());

        mk_converted_type_value(test_value, res).unwrap();
    }

    #[test]
    fn test_standard_community_2() {
        let test_value: Community = 
            StandardCommunity::new(
                Asn16::from(12500),
                Tag::new(7890),
            ).into();
        let res = StringLiteral::new("AS12500:7890".to_string());

        mk_converted_type_value(test_value, res).unwrap();
    }

    #[test]
    fn test_standard_community_3() -> Result<(), CompileError> {
        init();

        let test_value: Community =
                StandardCommunity::new(
                    Asn16::from(12500),
                    Tag::new(7890),
                ).into();
        let res: Community = 
            StandardCommunity::new(
                Asn16::from(7500),
                Tag::new(3000),
            ).into();

        test_consume_method_on_type_value(test_value, "set", res)
    }

    #[test]
    fn test_ext_community_1() -> Result<(), CompileError> {
        init();
        
        let test_value: Community =
                ExtendedCommunity::from_str(
                    "ro:123:456"
                ).unwrap().into();
        let res: Community = 
            StandardCommunity::new(
                Asn16::from(7500),
                Tag::new(3000),
            ).into();

        test_consume_method_on_type_value(test_value, "set", res)
    }

    #[test]
    fn test_large_community_1() -> Result<(), CompileError> {
        init();
        
        let test_value: Community =
                LargeCommunity::from_str(
                    "234:123:456"
                ).unwrap().into();
        let res: Community =
            StandardCommunity::new(
                Asn16::from(7500),
                Tag::new(3000),
            ).into();

        test_consume_method_on_type_value(test_value, "set", res)
    }

    #[test]
    #[should_panic = "failed to parse global admin part"]
    fn test_large_community_2() {
        init();
        
        let test_value: Community = LargeCommunity::from_str(
                    "2456850985534:123:456"
                ).unwrap().into();
        let res: Community = 
            StandardCommunity::new(
                Asn16::from(7500),
                Tag::new(3000),
            ).into();

        test_consume_method_on_type_value(test_value, "set", res).unwrap();
    }
    
    //------------ Test: IpAddr ----------------------------------------------

    #[test]
    fn test_ip_address_literal_1() -> Result<(), CompileError> {
        init();

        let test_value = IpAddr::try_from(&IpAddressLiteral("24.0.2.0".to_string())).unwrap();
        let res = std::net::IpAddr::from([24,0,2,0]);
        mk_converted_type_value(test_value, res)
    }

    #[test]
    fn test_ip_address_literal_2() -> Result<(), CompileError> {
        init();

        let test_value = IpAddr::try_from(&IpAddressLiteral("24.0.2.0".to_string())).unwrap();
        let res = StringLiteral("24.0.2.0".into());
        mk_converted_type_value(test_value, res)
    }

    #[test]
    fn test_ip_address_literal_3() -> Result<(), CompileError> {
        init();

        let test_value = IpAddr::try_from(&IpAddressLiteral("2001::ffff".to_string())).unwrap();
        let res = std::net::IpAddr::from([0x2001,0x0,0x0,0x0,0x0,0x0,0x0,0xffff]);

        assert_eq!(TypeValue::from(test_value).into_builtin()?, BuiltinTypeValue::IpAddr(res));
        mk_converted_type_value(test_value, res)
    }

    //-------- Test: Asn -----------------------------------------------------

    #[test]
    fn test_asn_1() -> Result<(), CompileError> {
        init();

        let test_value = Asn::from(&AsnLiteral(65534));
        let res = StringLiteral::from("AS65534");

        assert_eq!(TypeValue::from(test_value).into_builtin()?, BuiltinTypeValue::Asn(Asn::from(65534)));
        mk_converted_type_value(test_value, res)
    }

    #[test]
    #[should_panic = r#"Result::unwrap()` on an `Err` value: User("Cannot convert an instance of type Asn with a value 65534 into type U8. It's greater than 255")"#]
    fn test_asn_2() {
        init();

        let test_value = Asn::from(&AsnLiteral(65534));
        let _res = test_value.into_type(&TypeDef::U8).unwrap();
    }
}