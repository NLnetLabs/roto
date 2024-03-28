use inetnum::addr::Prefix;
use routecore::bgp::nlri::afisafi::IsPrefix;
use routecore::bgp::nlri::afisafi::NlriType;
use routecore::bgp::types::PathId;
use routecore::bgp::nlri::afisafi::AfiSafiType;
use routecore::{
    bgp::{
        message::UpdateMessage,
        nlri::afisafi::{
            Ipv4FlowSpecNlri, Ipv4MulticastAddpathNlri,
            Ipv4MulticastNlri, Ipv4UnicastAddpathNlri, Ipv4UnicastNlri,
            Ipv6FlowSpecNlri, Ipv6MulticastAddpathNlri, Ipv6MulticastNlri,
            Ipv6UnicastAddpathNlri, Ipv6UnicastNlri,
        },
        path_attributes::PaMap,
        workshop::route::RouteWorkshop,
        ParseError,
    },
    Octets,
};
use serde::Serialize;

use crate::types::typevalue::TypeValue;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub enum PrefixRouteWs {
    Ipv4Unicast(RouteWorkshop<Ipv4UnicastNlri>),
    Ipv4UnicastAddpath(RouteWorkshop<Ipv4UnicastAddpathNlri>),
    Ipv6Unicast(RouteWorkshop<Ipv6UnicastNlri>),
    Ipv6UnicastAddpath(RouteWorkshop<Ipv6UnicastAddpathNlri>),
    Ipv4Multicast(RouteWorkshop<Ipv4MulticastNlri>),
    Ipv4MulticastAddpath(RouteWorkshop<Ipv4MulticastAddpathNlri>),
    Ipv6Multicast(RouteWorkshop<Ipv6MulticastNlri>),
    Ipv6MulticastAddpath(RouteWorkshop<Ipv6MulticastAddpathNlri>)
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct PrefixNlri {
    ty: AfiSafiType,
    prefix: Prefix,
    path_id: Option<PathId>
}

impl PrefixNlri {
    pub fn prefix(&self) -> Prefix {
        self.prefix
    }

    pub fn path_id(&self) -> Option<PathId> {
        self.path_id
    }

    pub fn get_type(&self) -> AfiSafiType {
        self.ty
    }
}

impl From<&Ipv4UnicastNlri> for PrefixNlri {
    fn from(value: &Ipv4UnicastNlri) -> PrefixNlri {
        PrefixNlri {
            ty: AfiSafiType::Ipv4Unicast,
            prefix: value.prefix(),
            path_id: None
        }
    }
}

impl From<&Ipv4UnicastAddpathNlri> for PrefixNlri {
    fn from(value: &Ipv4UnicastAddpathNlri) -> PrefixNlri {
        PrefixNlri {
            ty: AfiSafiType::Ipv4Unicast,
            prefix: value.prefix(),
            path_id: value.path_id()
        }
    }
}

impl From<&Ipv6UnicastNlri> for PrefixNlri {
    fn from(value: &Ipv6UnicastNlri) -> PrefixNlri {
        PrefixNlri {
            ty: AfiSafiType::Ipv6Unicast,
            prefix: value.prefix(),
            path_id: None
        }
    }
}

impl From<&Ipv6UnicastAddpathNlri> for PrefixNlri {
    fn from(value: &Ipv6UnicastAddpathNlri) -> PrefixNlri {
        PrefixNlri {
            ty: AfiSafiType::Ipv6Unicast,
            prefix: value.prefix(),
            path_id: value.path_id()
        }
    }
}

impl From<&Ipv4MulticastNlri> for PrefixNlri {
    fn from(value: &Ipv4MulticastNlri) -> PrefixNlri {
        PrefixNlri {
            ty: AfiSafiType::Ipv4Multicast,
            prefix: value.prefix(),
            path_id: None
        }
    }
}

impl From<&Ipv4MulticastAddpathNlri> for PrefixNlri {
    fn from(value: &Ipv4MulticastAddpathNlri) -> PrefixNlri {
        PrefixNlri {
            ty: AfiSafiType::Ipv4Multicast,
            prefix: value.prefix(),
            path_id: value.path_id()
        }
    }
}

impl From<&Ipv6MulticastNlri> for PrefixNlri {
    fn from(value: &Ipv6MulticastNlri) -> PrefixNlri {
        PrefixNlri {
            ty: AfiSafiType::Ipv6Multicast,
            prefix: value.prefix(),
            path_id: None
        }
    }
}

impl From<&Ipv6MulticastAddpathNlri> for PrefixNlri {
    fn from(value: &Ipv6MulticastAddpathNlri) -> PrefixNlri {
        PrefixNlri {
            ty: AfiSafiType::Ipv6Multicast,
            prefix: value.prefix(),
            path_id: value.path_id()
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct PrefixRoute(pub PrefixRouteWs);

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub enum FlowSpecNlri<O: Octets> {
    Ipv4FlowSpec(Ipv4FlowSpecNlri<O>),
    Ipv6FlowSpec(Ipv6FlowSpecNlri<O>)
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct FlowSpecRoute<O: routecore::Octets> {
    pub(crate) nlri: FlowSpecNlri<O>,
    pub(crate) attributes: PaMap,
}

impl From<RouteWorkshop<Ipv4UnicastNlri>> for PrefixRoute {
    fn from(value: RouteWorkshop<Ipv4UnicastNlri>) -> Self {
        PrefixRoute(PrefixRouteWs::Ipv4Unicast(value))
    }
}

impl From<RouteWorkshop<Ipv6UnicastNlri>> for PrefixRoute {
    fn from(value: RouteWorkshop<Ipv6UnicastNlri>) -> Self {
        PrefixRoute(PrefixRouteWs::Ipv6Unicast(value))
    }
}

impl From<RouteWorkshop<Ipv4UnicastAddpathNlri>> for PrefixRoute {
    fn from(value: RouteWorkshop<Ipv4UnicastAddpathNlri>) -> Self {
        PrefixRoute(PrefixRouteWs::Ipv4UnicastAddpath(value))
    }
}

impl From<RouteWorkshop<Ipv6UnicastAddpathNlri>> for PrefixRoute {
    fn from(value: RouteWorkshop<Ipv6UnicastAddpathNlri>) -> Self {
        PrefixRoute(PrefixRouteWs::Ipv6UnicastAddpath(value))
    }
}

impl From<RouteWorkshop<Ipv4MulticastAddpathNlri>> for PrefixRoute {
    fn from(value: RouteWorkshop<Ipv4MulticastAddpathNlri>) -> Self {
        PrefixRoute(PrefixRouteWs::Ipv4MulticastAddpath(value))
    }
}

impl From<RouteWorkshop<Ipv6MulticastAddpathNlri>> for PrefixRoute {
    fn from(value: RouteWorkshop<Ipv6MulticastAddpathNlri>) -> Self {
        PrefixRoute(PrefixRouteWs::Ipv6MulticastAddpath(value))
    }
}

pub fn explode_announcements(
    update: &UpdateMessage<bytes::Bytes>,
) -> Result<Vec<TypeValue>, ParseError> {
    let pa_map = PaMap::from_update_pdu(update).unwrap();
    let announce_afi_safis = update.announcement_fams();
    let mut res = vec![];

    for afi_safi in announce_afi_safis {
        match afi_safi {
            NlriType::Ipv4Unicast => {
                let iter = update
                    .typed_announcements::<_, Ipv4UnicastNlri>()
                    .unwrap()
                    .unwrap();

                res.extend(
                    iter.map(|a| {
                        TypeValue::from(
                            RouteWorkshop::from_pa_map(
                                a.unwrap(),
                                pa_map.clone(),
                            )
                        )
                    })
                    .collect::<Vec<_>>(),
                );
            }
            NlriType::Ipv4UnicastAddpath => {
                let iter = update
                    .typed_announcements::<_, Ipv4UnicastAddpathNlri>()
                    .unwrap()
                    .unwrap();

                res.extend(
                    iter.map(|a| {
                        RouteWorkshop::from_pa_map(
                            a.unwrap(),
                            pa_map.clone(),
                        ).into()
                    })
                    .collect::<Vec<_>>(),
                );
            }
            NlriType::Ipv6Unicast => {
                let iter = update
                    .typed_announcements::<_, Ipv6UnicastNlri>()
                    .unwrap()
                    .unwrap();
                res.extend(
                    iter.map(|a| {
                        RouteWorkshop::from_pa_map(
                            a.unwrap(),
                            pa_map.clone(),
                        ).into()
                    })
                    .collect::<Vec<_>>(),
                );
            }
            NlriType::Ipv6UnicastAddpath => {
                let iter = update
                    .typed_announcements::<_, Ipv6UnicastAddpathNlri>()
                    .unwrap()
                    .unwrap();
                res.extend(
                    iter.map(|a| {
                        RouteWorkshop::from_pa_map(
                            a.unwrap(),
                            pa_map.clone(),
                        ).into()
                    })
                    .collect::<Vec<_>>(),
                );
            }
            NlriType::Ipv4Multicast => {
                let iter = update
                    .typed_announcements::<_, Ipv4MulticastNlri>()
                    .unwrap()
                    .unwrap();
                res.extend(
                    iter.map(|a| {
                        RouteWorkshop::from_pa_map(
                            a.unwrap(),
                            pa_map.clone(),
                        ).into()
                    })
                    .collect::<Vec<_>>(),
                );
            }
            NlriType::Ipv4MulticastAddpath => {
                let iter = update
                    .typed_announcements::<_, Ipv4MulticastAddpathNlri>()
                    .unwrap()
                    .unwrap();
                res.extend(
                    iter.map(|a| {
                        RouteWorkshop::from_pa_map(
                            a.unwrap(),
                            pa_map.clone(),
                        ).into()
                    })
                    .collect::<Vec<_>>(),
                );
            }
            NlriType::Ipv6Multicast => {
                let iter = update
                    .typed_announcements::<_, Ipv6MulticastNlri>()
                    .unwrap()
                    .unwrap();
                res.extend(
                    iter.map(|a| {
                        RouteWorkshop::from_pa_map(
                            a.unwrap(),
                            pa_map.clone(),
                        ).into()
                    })
                    .collect::<Vec<_>>(),
                );
            }
            NlriType::Ipv6MulticastAddpath => {
                let iter = update
                    .typed_announcements::<_, Ipv6MulticastAddpathNlri>()
                    .unwrap()
                    .unwrap();
                res.extend(
                    iter.map(|a| {
                        RouteWorkshop::from_pa_map(
                            a.unwrap(),
                            pa_map.clone(),
                        ).into()
                    })
                    .collect::<Vec<_>>(),
                );
            }
            NlriType::Ipv4MplsUnicast => todo!(),
            NlriType::Ipv4MplsUnicastAddpath => todo!(),
            NlriType::Ipv6MplsUnicast => todo!(),
            NlriType::Ipv6MplsUnicastAddpath => todo!(),
            NlriType::Ipv4MplsVpnUnicast => todo!(),
            NlriType::Ipv4MplsVpnUnicastAddpath => todo!(),
            NlriType::Ipv6MplsVpnUnicast => todo!(),
            NlriType::Ipv6MplsVpnUnicastAddpath => todo!(),
            NlriType::Ipv4RouteTarget => todo!(),
            NlriType::Ipv4RouteTargetAddpath => todo!(),
            NlriType::Ipv4FlowSpec => {
                let iter = update
                    .typed_announcements::<_, Ipv4FlowSpecNlri<bytes::Bytes>>()
                    .unwrap()
                    .unwrap();
                res.extend(
                    iter.map(|a| {
                        TypeValue::Builtin(super::BuiltinTypeValue::FlowSpecRoute(
                            FlowSpecRoute {
                                nlri: FlowSpecNlri::Ipv4FlowSpec(a.unwrap()),
                                attributes: pa_map.clone()
                            }
                        ))
                    })
                    .collect::<Vec<_>>(),
                );
            }
            NlriType::Ipv4FlowSpecAddpath => todo!(),
            NlriType::Ipv6FlowSpec => {
                let iter = update
                    .typed_announcements::<_, Ipv6FlowSpecNlri<bytes::Bytes>>()
                    .unwrap()
                    .unwrap();
                res.extend(
                    iter.map(|a| {
                        RouteWorkshop::from_pa_map(
                            a.unwrap(),
                            pa_map.clone(),
                        ).into()
                    })
                    .collect::<Vec<_>>(),
                );
            }
            NlriType::Ipv6FlowSpecAddpath => todo!(),
            NlriType::L2VpnVpls => todo!(),
            NlriType::L2VpnVplsAddpath => todo!(),
            NlriType::L2VpnEvpn => todo!(),
            NlriType::L2VpnEvpnAddpath => todo!(),
            NlriType::Unsupported(_, _) => todo!(),
        };
    }
    Ok(res)
}

#[rustfmt::skip]
#[test]
    fn pdu_into_rws_typed() {

        use crate::types::typevalue::TypeValue;
        use crate::types::builtin::BuiltinTypeValue;
        use routecore::bgp::message::SessionConfig;
        use crate::types::builtin::route::NlriType::{Ipv4Unicast, Ipv6Unicast};

        // UPDATE with 5 ipv6 nlri + 2 conventional
        let raw = bytes::Bytes::from(vec![
            0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
            0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
            //0x00, 0x88,
            0x00, 0x88 + 6,
            0x02, 0x00, 0x00, 0x00, 0x71, 0x80,
            0x0e, 0x5a, 0x00, 0x02, 0x01, 0x20, 0xfc, 0x00,
            0x00, 0x10, 0x00, 0x01, 0x00, 0x10, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0xfe, 0x80,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x00, 0x80,
            0xfc, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10,
            0x40, 0x20, 0x01, 0x0d, 0xb8, 0xff, 0xff, 0x00,
            0x00, 0x40, 0x20, 0x01, 0x0d, 0xb8, 0xff, 0xff,
            0x00, 0x01, 0x40, 0x20, 0x01, 0x0d, 0xb8, 0xff,
            0xff, 0x00, 0x02, 0x40, 0x20, 0x01, 0x0d, 0xb8,
            0xff, 0xff, 0x00, 0x03, 0x40, 0x01, 0x01, 0x00,
            0x40, 0x02, 0x06, 0x02, 0x01, 0x00, 0x00, 0x00,
            0xc8, 0x80, 0x04, 0x04, 0x00, 0x00, 0x00, 0x00,
            16, 1, 2,
            16, 10, 20

        ]);
        let pdu = UpdateMessage::from_octets(raw, &SessionConfig::modern())
            .unwrap();

        let announces = pdu.announcement_fams().collect::<Vec<_>>();
        assert_eq!(announces, [Ipv4Unicast, Ipv6Unicast]);

        let res = explode_announcements(&pdu).unwrap();

        // let res = pdu_into_typed_rws::<_, RouteWorkshop<BasicNlri>, _, Ipv6UnicastNlri>(&pdu);
        let mut ipv4_nlri = 0;
        let mut ipv6_nlri = 0;
        for rws in &res {
            println!("{}", rws);

            if let TypeValue::Builtin(BuiltinTypeValue::PrefixRoute(route)) = rws {
                match route.nlri().get_type() {
                    AfiSafiType::Ipv4Unicast => { ipv4_nlri += 1; },
                    AfiSafiType::Ipv6Unicast => { ipv6_nlri += 1; },
                    _ => {}
                }
            }
        }
        assert_eq!(ipv4_nlri, 2);
        assert_eq!(ipv6_nlri, 5);
        assert_eq!(res.len(), 7);

        // let res = pdu_into_typed_rws::<_, RouteWorkshop<BasicNlri>, _, Ipv4UnicastNlri>(&pdu);
        // for rws in &res {
        //     println!("{}", rws.nlri());
        // }
        // assert_eq!(res.len(), 2);

        // let res = pdu_into_typed_rws::<_, RouteWorkshop<_>, _, Ipv4FlowSpecNlri<_>>(&pdu);
        // for rws in &res {
        //     println!("{}", rws.nlri());
        // }
        // assert_eq!(res.len(), 0);
    }
