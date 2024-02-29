#[cfg(test)]
mod route {
    use std::net::{IpAddr, Ipv4Addr};

    use super::super::{
        IntegerLiteral, PrefixLength, StringLiteral,
    };
    use crate::ast::{IpAddressLiteral, AsnLiteral};
    use crate::types::builtin::basic_route::{PeerId, PeerRibType, Provenance};
    use crate::types::builtin::{BytesRecord, BuiltinTypeValue};
    use log::trace;
    use routecore::bgp::message::update_builder::MpReachNlriBuilder;
    use routecore::bgp::path_attributes::FromAttribute;
    use routecore::bgp::workshop::route::RouteWorkshop;
    use routecore::bgp::workshop::route::WorkshopAttribute;
    use crate::types::lazyrecord_types::BgpUpdateMessage;
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
    use routecore::bgp::aspath::HopPath as AsPath;
    use routecore::asn::{Asn, Asn16};
    use routecore::bgp::communities::{Community, HumanReadableCommunity, StandardCommunity, Tag, ExtendedCommunity, LargeCommunity};
    use routecore::bgp::types::{LocalPref, MultiExitDisc, NextHop, Origin};
    use routecore::bgp::{
        message::{
            nlri::{BasicNlri, Nlri},
            SessionConfig,
        },
        types::OriginType,
    };

    use std::str::FromStr;

    use crate::vm::VmError;

    use std::io::Write;

    pub fn init() {
        let _ = env_logger::builder()
            .format(|buf, record| writeln!(buf, "{}", record.args()))
            .is_test(true)
            .try_init();
    }

    // type AsPath = CoreAsPath<bytes::Bytes>;

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

        let update: BytesRecord<BgpUpdateMessage> =
            BytesRecord::<BgpUpdateMessage>::new(buf, SessionConfig::modern()).unwrap();

        let prefixes: Vec<routecore::addr::Prefix> = update
            .bytes_parser()
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

        let mut roto_msgs = vec![];

        let provenance = Provenance {
            timestamp: chrono::Utc::now(),
            router_id: 0,
            connection_id: 0,
            peer_id: PeerId { addr: "172.0.0.1".parse().unwrap(), asn: Asn::from(65530)},
            peer_bgp_id: [0,0,0,0].into(),
            peer_distuingisher: [0; 8],
            peer_rib_type: PeerRibType::OutPost,
        };

        let nlri = BasicNlri { prefix: prefixes[0], path_id: None };
        let bgp_msg = update.into_inner();

        roto_msgs.push(RouteWorkshop::from_update_pdu(
            nlri,
            &bgp_msg
        ).unwrap());

        for prefix in &prefixes[1..] {
            let nlri = BasicNlri { prefix: *prefix, path_id: None };

            roto_msgs.push(RouteWorkshop::from_update_pdu(
                nlri,
                &bgp_msg
                // routecore::bgp::types::AfiSafi::Ipv6Unicast,
                // None,
                // NlriStatus::InConvergence,
                // provenance
            ).unwrap());
        }

        println!("{:?}", roto_msgs);
        println!("{:#?}", roto_msgs[2]);

        assert_eq!(roto_msgs[0].nlri().prefix(), prefixes[0]);
        assert_eq!(roto_msgs[1].nlri().prefix(), prefixes[1]);
        assert_eq!(roto_msgs[2].nlri().prefix(), prefixes[2]);
        assert_eq!(roto_msgs[3].nlri().prefix(), prefixes[3]);
        assert_eq!(roto_msgs[4].nlri().prefix(), prefixes[4]);
        assert_eq!(roto_msgs.len(), 5);

        // let mut delta = roto_msgs[0].open_new_delta(delta_id)?;
        let mut path_attrs = roto_msgs.remove(0);
        if let std::net::IpAddr::V6(v6) = prefixes[0].addr() {

        let next_hop = routecore::bgp::types::NextHop::Unicast(prefixes[0].addr());
        let _ = path_attrs.set_attr::<routecore::bgp::types::NextHop>(next_hop);
        }

        let asp = path_attrs.get_attr::<AsPath>();
        assert!(asp.is_some());
        let mut asp = asp.unwrap();
        asp.prepend(Asn::from(211321_u32));
        let _ = path_attrs.set_attr(asp);

        let _ = path_attrs.set_attr(Origin(OriginType::Incomplete));
        let _ = path_attrs.set_attr(LocalPref(100));
        let res: Option<Origin> = path_attrs.get_attr::<Origin>();
        assert_eq!(res, Some(Origin(OriginType::Igp)));

        let as_path = path_attrs.get_attr::<AsPath>().unwrap();
        println!("change set {:?}", path_attrs);
        println!("as_path {:?}", as_path);

        let mut communities = path_attrs.get_attr::<Vec<routecore::bgp::communities::Community>>().unwrap();
        communities.push(StandardCommunity::from_u32(666).into());
        communities.push(ExtendedCommunity::from_str("rt: 192.0.2.0:65536").unwrap().into());
        let _ = path_attrs.set_attr(communities);

        let mut std_communities = path_attrs.get_attr::<Vec<Community>>().unwrap();

        let _ = path_attrs.set_attr(routecore::bgp::types::OriginatorId(Ipv4Addr::from([127,0,0,1])));
        // let mut msg_1 = roto_msgs.remove(1);
        // msg_1.store_attrs(path_attrs);

        // println!("materialize! {:#?}", msg_1.clone_attrs());

        // let attr_set = msg_1.clone_attrs();
        assert_eq!(path_attrs.get_attr::<AsPath>().map(|asp| asp.hop_count()), Some(2));

        println!(
            "ATTR_SET_AS_PATH {:?}",
            path_attrs.get_attr::<MultiExitDisc>().unwrap()
        );
        assert_eq!(
            path_attrs.get_attr::<AsPath>().unwrap().origin().unwrap(),
            &routecore::bgp::aspath::Hop::from(Asn::from(211321_u32))
        );
        assert_eq!(
            path_attrs.get_attr::<AsPath>().unwrap().get_hop(1).unwrap(),
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

        let update =
            BytesRecord::<BgpUpdateMessage>::new(buf, SessionConfig::modern()).unwrap();

        let prefixes: Vec<routecore::addr::Prefix> = update
            .bytes_parser()
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

        let bgp_msg = update.into_inner();
        let mut roto_msgs = vec![];

        roto_msgs.push(RouteWorkshop::from_update_pdu(
            BasicNlri { prefix: prefixes[0], path_id: None },
            &bgp_msg
        ).unwrap());

        for prefix in &prefixes[1..] {
            roto_msgs.push(RouteWorkshop::from_update_pdu(
                BasicNlri { prefix: *prefix, path_id: None },
                &bgp_msg
            ).unwrap())
        }

        assert_eq!(roto_msgs[0].nlri().prefix(), prefixes[0]);
        assert_eq!(roto_msgs[1].nlri().prefix(), prefixes[1]);
        assert_eq!(roto_msgs[2].nlri().prefix(), prefixes[2]);
        assert_eq!(roto_msgs[3].nlri().prefix(), prefixes[3]);
        assert_eq!(roto_msgs[4].nlri().prefix(), prefixes[4]);
        assert_eq!(roto_msgs.len(), 5);

        let attr_set = &mut roto_msgs[2];
        let mp_reach = MpReachNlriBuilder::new_for_nlri(&Nlri::Unicast::<bytes::Bytes>(*attr_set.nlri()));
        attr_set.set_attr(mp_reach).unwrap();

        println!("change set {:#?}", attr_set);
        if let std::net::IpAddr::V6(v6) = prefixes[2].addr() {
            let next_hop = NextHop::Unicast(std::net::IpAddr::V6(v6));
            attr_set
                .set_attr::<NextHop>(next_hop).unwrap();
        }

        let mut as_path = attr_set.get_attr::<AsPath>().unwrap();
        as_path.prepend(Asn::from(211321_u32));
        attr_set.set_attr::<AsPath>(as_path).unwrap();
        
        let res = attr_set.get_attr::<AsPath>();
        assert!(res.is_some());

        attr_set
            .set_attr(Origin(routecore::bgp::types::OriginType::Incomplete)).unwrap();
        let res = attr_set.get_attr::<Origin>();
        assert_eq!(res, Some(Origin(OriginType::Incomplete)));

        assert_eq!(attr_set.get_attr::<AsPath>().unwrap().hop_count(), 2_usize);
        assert_eq!(
            attr_set.get_attr::<AsPath>().unwrap().get_hop(0).unwrap(),
            &routecore::bgp::aspath::Hop::from(
                Asn::from(211321_u32)
            )
        );
        assert_eq!(
            attr_set.get_attr::<AsPath>().unwrap().get_hop(1).unwrap(),
            &routecore::bgp::aspath::Hop::from(Asn::from(200_u32))
        );

        let mut as_path = attr_set.get_attr::<AsPath>().unwrap();
        as_path.prepend(Asn::from(211322_u32));
        attr_set.set_attr(as_path).unwrap();

        let res = attr_set.get_attr::<AsPath>();
        assert!(res.is_some());

        assert_eq!(attr_set.get_attr::<AsPath>().map(|asp| asp.hop_count()), Some(3));

        assert_eq!(
            attr_set.get_attr::<AsPath>().unwrap().get_hop(0).unwrap(),
            &routecore::bgp::aspath::Hop::from(Asn::from(211322_u32))
        );
        assert_eq!(
            attr_set.get_attr::<AsPath>().unwrap().get_hop(1).unwrap(),
            &routecore::bgp::aspath::Hop::from(Asn::from(211321_u32))
        );
        assert_eq!(
            attr_set.get_attr::<AsPath>().unwrap().origin().unwrap(),
            &routecore::bgp::aspath::Hop::from(routecore::asn::Asn::from(200_u32))
        );

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

        use crate::{types::builtin::BuiltinTypeValue, vm::StackValue};
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
        let arg: routecore::bgp::aspath::HopPath = vec![Asn::from(24)].into();

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
        let res: routecore::bgp::aspath::HopPath = vec![Asn::from(24)].into();

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
        let test_value: HumanReadableCommunity = HumanReadableCommunity::from_str("AS1234:7890").unwrap();
        let res = StringLiteral::new("AS1234:7890".to_string());

        mk_converted_type_value(test_value, res).unwrap();
    }

    #[test]
    fn test_standard_community_2() {
        let test_value: HumanReadableCommunity = 
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

        let test_value: HumanReadableCommunity =
                StandardCommunity::new(
                    Asn16::from(12500),
                    Tag::new(7890),
                ).into();
        let res: HumanReadableCommunity = 
            StandardCommunity::new(
                Asn16::from(7500),
                Tag::new(3000),
            ).into();

        test_consume_method_on_type_value(test_value, "set", res)
    }

    #[test]
    fn test_ext_community_1() -> Result<(), CompileError> {
        init();
        
        let test_value: HumanReadableCommunity =
                ExtendedCommunity::from_str(
                    "ro:123:456"
                ).unwrap().into();
        let res: HumanReadableCommunity = 
            StandardCommunity::new(
                Asn16::from(7500),
                Tag::new(3000),
            ).into();

        test_consume_method_on_type_value(test_value, "set", res)
    }

    #[test]
    fn test_large_community_1() -> Result<(), CompileError> {
        init();
        
        let test_value: HumanReadableCommunity =
                LargeCommunity::from_str(
                    "234:123:456"
                ).unwrap().into();
        let res: HumanReadableCommunity =
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
        
        let test_value: HumanReadableCommunity = LargeCommunity::from_str(
                    "2456850985534:123:456"
                ).unwrap().into();
        let res: HumanReadableCommunity = 
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