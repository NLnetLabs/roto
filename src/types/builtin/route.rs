use std::collections::BTreeSet;

use inetnum::addr::Prefix;
use routecore::bgp::nlri::afisafi::AfiSafiType;
use routecore::bgp::nlri::afisafi::IsPrefix;
use routecore::bgp::nlri::afisafi::NlriType;
use routecore::bgp::types::PathId;
use routecore::{
    bgp::{
        message::UpdateMessage,
        nlri::afisafi::{
            Ipv4FlowSpecNlri, Ipv4MulticastAddpathNlri, Ipv4MulticastNlri,
            Ipv4UnicastAddpathNlri, Ipv4UnicastNlri, Ipv6FlowSpecNlri,
            Ipv6MulticastAddpathNlri, Ipv6MulticastNlri,
            Ipv6UnicastAddpathNlri, Ipv6UnicastNlri,
        },
        path_attributes::PaMap,
        workshop::route::RouteWorkshop,
        ParseError,
    },
    // Octets,
};
use serde::Serialize;

use crate::types::typevalue::TypeValue;

use super::Nlri;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub enum PrefixRouteWs {
    Ipv4Unicast(RouteWorkshop<Ipv4UnicastNlri>),
    Ipv4UnicastAddpath(RouteWorkshop<Ipv4UnicastAddpathNlri>),
    Ipv6Unicast(RouteWorkshop<Ipv6UnicastNlri>),
    Ipv6UnicastAddpath(RouteWorkshop<Ipv6UnicastAddpathNlri>),
    Ipv4Multicast(RouteWorkshop<Ipv4MulticastNlri>),
    Ipv4MulticastAddpath(RouteWorkshop<Ipv4MulticastAddpathNlri>),
    Ipv6Multicast(RouteWorkshop<Ipv6MulticastNlri>),
    Ipv6MulticastAddpath(RouteWorkshop<Ipv6MulticastAddpathNlri>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct PrefixNlri {
    ty: AfiSafiType,
    prefix: Prefix,
    path_id: Option<PathId>,
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
            path_id: None,
        }
    }
}

impl From<&Ipv4UnicastAddpathNlri> for PrefixNlri {
    fn from(value: &Ipv4UnicastAddpathNlri) -> PrefixNlri {
        PrefixNlri {
            ty: AfiSafiType::Ipv4Unicast,
            prefix: value.prefix(),
            path_id: value.path_id(),
        }
    }
}

impl From<&Ipv6UnicastNlri> for PrefixNlri {
    fn from(value: &Ipv6UnicastNlri) -> PrefixNlri {
        PrefixNlri {
            ty: AfiSafiType::Ipv6Unicast,
            prefix: value.prefix(),
            path_id: None,
        }
    }
}

impl From<&Ipv6UnicastAddpathNlri> for PrefixNlri {
    fn from(value: &Ipv6UnicastAddpathNlri) -> PrefixNlri {
        PrefixNlri {
            ty: AfiSafiType::Ipv6Unicast,
            prefix: value.prefix(),
            path_id: value.path_id(),
        }
    }
}

impl From<&Ipv4MulticastNlri> for PrefixNlri {
    fn from(value: &Ipv4MulticastNlri) -> PrefixNlri {
        PrefixNlri {
            ty: AfiSafiType::Ipv4Multicast,
            prefix: value.prefix(),
            path_id: None,
        }
    }
}

impl From<&Ipv4MulticastAddpathNlri> for PrefixNlri {
    fn from(value: &Ipv4MulticastAddpathNlri) -> PrefixNlri {
        PrefixNlri {
            ty: AfiSafiType::Ipv4Multicast,
            prefix: value.prefix(),
            path_id: value.path_id(),
        }
    }
}

impl From<&Ipv6MulticastNlri> for PrefixNlri {
    fn from(value: &Ipv6MulticastNlri) -> PrefixNlri {
        PrefixNlri {
            ty: AfiSafiType::Ipv6Multicast,
            prefix: value.prefix(),
            path_id: None,
        }
    }
}

impl From<&Ipv6MulticastAddpathNlri> for PrefixNlri {
    fn from(value: &Ipv6MulticastAddpathNlri) -> PrefixNlri {
        PrefixNlri {
            ty: AfiSafiType::Ipv6Multicast,
            prefix: value.prefix(),
            path_id: value.path_id(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct PrefixRoute(pub PrefixRouteWs);

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub enum FlowSpecNlri<O: AsRef<[u8]>> {
    Ipv4FlowSpec(Ipv4FlowSpecNlri<O>),
    Ipv6FlowSpec(Ipv6FlowSpecNlri<O>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct FlowSpecRoute<O: AsRef<[u8]>> {
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

macro_rules! announcements_into_typevalues {
    ( $nlri_type:ty, $update: ident, $res: ident, $nlri_set: ident) => {
        let iter = if let Some(iter) =
            $update.typed_announcements::<_, $nlri_type>()?
        {
            iter
        } else {
            return Err(ParseError::ShortInput);
        };

        $res.extend(
            iter.filter_map(|a| {
                if let Ok(a) = a {
                    $nlri_set.insert(Nlri::from(a.clone()));
                    RouteWorkshop::from_update_pdu(a, $update)
                        .ok()
                        .map(|a| a.into())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>(),
        );
    };
}

/// Create a `Vec<TypeValue::Route<RouteWorkshop<_>>` from a PDU that
/// represents a BGP [`routecore::UpdateMessage`]. The returned Vec will
/// contain one TypeValue-wrapped RWS for every announcement in the PDU, no
/// matter what the type of the `NLRI` of the announcement is.
#[allow(clippy::mutable_key_type)]
pub fn explode_announcements(
    update: &UpdateMessage<bytes::Bytes>,
    nlri_set: &mut BTreeSet<Nlri>,
) -> Result<Vec<TypeValue>, ParseError> {
    // Read all the types of NLRI in MP_REACH and/or conventional NLRI.
    let announce_afi_safis = update.announcement_fams();

    let mut tv_vec = vec![];

    for afi_safi in announce_afi_safis {
        match afi_safi {
            NlriType::Ipv4Unicast => {
                announcements_into_typevalues!(Ipv4UnicastNlri, update, tv_vec, nlri_set);
            }
            NlriType::Ipv4UnicastAddpath => {
                announcements_into_typevalues!(
                    Ipv4UnicastAddpathNlri,
                    update,
                    tv_vec,
                    nlri_set
                );
            }
            NlriType::Ipv6Unicast => {
                announcements_into_typevalues!(Ipv6UnicastNlri, update, tv_vec, nlri_set);
            }
            NlriType::Ipv6UnicastAddpath => {
                announcements_into_typevalues!(
                    Ipv6UnicastAddpathNlri,
                    update,
                    tv_vec,
                    nlri_set
                );
            }
            NlriType::Ipv4Multicast => {
                announcements_into_typevalues!(
                    Ipv4MulticastNlri,
                    update,
                    tv_vec,
                    nlri_set
                );
            }
            NlriType::Ipv4MulticastAddpath => {
                announcements_into_typevalues!(
                    Ipv4MulticastAddpathNlri,
                    update,
                    tv_vec,
                    nlri_set
                );
            }
            NlriType::Ipv6Multicast => {
                announcements_into_typevalues!(
                    Ipv6MulticastNlri,
                    update,
                    tv_vec,
                    nlri_set
                );
            }
            NlriType::Ipv6MulticastAddpath => {
                announcements_into_typevalues!(
                    Ipv6MulticastAddpathNlri,
                    update,
                    tv_vec,
                    nlri_set
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
                announcements_into_typevalues!(
                    Ipv4FlowSpecNlri<bytes::Bytes>,
                    update,
                    tv_vec,
                    nlri_set
                );
            }
            NlriType::Ipv4FlowSpecAddpath => todo!(),
            NlriType::Ipv6FlowSpec => {
                announcements_into_typevalues!(
                    Ipv6FlowSpecNlri<bytes::Bytes>,
                    update,
                    tv_vec,
                    nlri_set
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
    Ok(tv_vec)
}

macro_rules! withdrawals_into_typevalues {
    ( $nlri_type:ty, $update: ident, $res: ident, $nlri_set: ident) => {
        let iter = if let Some(iter) =
            $update.typed_withdrawals::<_, $nlri_type>()?
        {
            iter
        } else {
            return Err(ParseError::ShortInput);
        };

        $res.extend(
            iter.filter_map(|a| {
                if let Ok(a) = a {
                    $nlri_set.insert(Nlri::from(a.clone()));
                    RouteWorkshop::from_update_pdu(a, $update)
                        .ok()
                        .map(|a| a.into())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>(),
        );
    };
}

/// Create a `Vec<TypeValue::Route<RouteWorkshop<_>>` from a PDU that
/// represents a BGP [`routecore::UpdateMessage`]. The returned Vec will
/// contain one TypeValue-wrapped RWS for every announcement in the PDU, no
/// matter what the type of the `NLRI` of the announcement is.
#[allow(clippy::mutable_key_type)]
pub fn explode_withdrawals(
    update: &UpdateMessage<bytes::Bytes>,
    nlri_set: &mut BTreeSet<Nlri>
) -> Result<Vec<TypeValue>, ParseError> {
    // Read all the types of NLRI in MP_REACH and/or conventional NLRI.
    let announce_afi_safis = update.withdrawal_fams();

    let mut res = vec![];

    for afi_safi in announce_afi_safis {
        match afi_safi {
            NlriType::Ipv4Unicast => {
                withdrawals_into_typevalues!(Ipv4UnicastNlri,
                    update, res, nlri_set);
            }
            NlriType::Ipv4UnicastAddpath => {
                withdrawals_into_typevalues!(
                    Ipv4UnicastAddpathNlri,
                    update,
                    res,
                    nlri_set
                );
            }
            NlriType::Ipv6Unicast => {
                withdrawals_into_typevalues!(Ipv6UnicastNlri, 
                    update, res, nlri_set);
            }
            NlriType::Ipv6UnicastAddpath => {
                withdrawals_into_typevalues!(
                    Ipv6UnicastAddpathNlri,
                    update,
                    res,
                    nlri_set
                );
            }
            NlriType::Ipv4Multicast => {
                withdrawals_into_typevalues!(Ipv4MulticastNlri, 
                    update, res, nlri_set);
            }
            NlriType::Ipv4MulticastAddpath => {
                withdrawals_into_typevalues!(
                    Ipv4MulticastAddpathNlri,
                    update,
                    res,
                    nlri_set
                );
            }
            NlriType::Ipv6Multicast => {
                withdrawals_into_typevalues!(Ipv6MulticastNlri, 
                    update, res, nlri_set);
            }
            NlriType::Ipv6MulticastAddpath => {
                withdrawals_into_typevalues!(
                    Ipv6MulticastAddpathNlri,
                    update,
                    res,
                    nlri_set
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
                withdrawals_into_typevalues!(
                    Ipv4FlowSpecNlri<bytes::Bytes>,
                    update,
                    res,
                    nlri_set
                );
            }
            NlriType::Ipv4FlowSpecAddpath => todo!(),
            NlriType::Ipv6FlowSpec => {
                withdrawals_into_typevalues!(
                    Ipv6FlowSpecNlri<bytes::Bytes>,
                    update,
                    res,
                    nlri_set
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
            0x00, 0x95,
            0x02, 0x00, 0x00, 0x00, 0x78,
            0x80,
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
            0xc8,
            0x40, 0x03, 0x04, 0x01, 0x02, 0x03, 0x04, // NEXT_HOP
            0x80, 0x04, 0x04, 0x00, 0x00, 0x00, 0x00,
            16, 1, 2,
            16, 10, 20
        ]);
        let pdu = UpdateMessage::from_octets(raw, &SessionConfig::modern())
            .unwrap();

        let announces = pdu.announcement_fams().collect::<Vec<_>>();

        #[allow(clippy::mutable_key_type)]
        let mut nlri_set = BTreeSet::new();

        assert_eq!(announces, [Ipv4Unicast, Ipv6Unicast]);

        let res = explode_announcements(&pdu, &mut nlri_set).unwrap();

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
