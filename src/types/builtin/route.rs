use inetnum::addr::Prefix;
use routecore::bgp::nlri::afisafi::Addpath;
use routecore::bgp::nlri::afisafi::IsPrefix;
use routecore::bgp::nlri::afisafi::Nlri;
use routecore::bgp::nlri::afisafi::NlriType;
use routecore::{
    bgp::{
        message::UpdateMessage,
        nlri::afisafi::{
            AfiSafiType, Ipv4FlowSpecNlri, Ipv4MulticastAddpathNlri,
            Ipv4MulticastNlri, Ipv4UnicastAddpathNlri, Ipv4UnicastNlri,
            Ipv6FlowSpecNlri, Ipv6MulticastAddpathNlri, Ipv6MulticastNlri,
            Ipv6UnicastAddpathNlri, Ipv6UnicastNlri,
        },
        path_attributes::PaMap,
        types::PathId,
        workshop::route::RouteWorkshop,
        ParseError,
    },
    Octets,
};
use serde::Serialize;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct BasicNlri {
    pub ty: AfiSafiType,
    pub prefix: Prefix,
    pub path_id: Option<PathId>,
}

impl BasicNlri {
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

impl std::fmt::Display for BasicNlri {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.prefix)
    }
}

impl routecore::bgp::nlri::afisafi::AfiSafi for BasicNlri {
    fn afi() -> routecore::bgp::types::Afi {
        unimplemented!()
    }

    fn afi_safi() -> AfiSafiType {
        unimplemented!()
    }
}

impl routecore::bgp::nlri::afisafi::AfiSafiNlri for BasicNlri {
    type Nlri = BasicNlri;
    
    fn nlri(&self) -> Self::Nlri {
        *self
    }
    
    fn afi_safi_type(&self) -> AfiSafiType {
        self.ty
    }
}

impl routecore::bgp::nlri::afisafi::IsNlri for BasicNlri {
    fn nlri_type() -> NlriType {
        unimplemented!()
    }
}

impl routecore::bgp::nlri::afisafi::NlriCompose for BasicNlri {
    fn compose<Target: routecore::OctetsBuilder>(&self, target: &mut Target)
        -> Result<(), Target::AppendError> {

        match self.ty {
            AfiSafiType::Ipv4Unicast => {
                match self.path_id {
                    Some(path_id) => {
                        let nlri: Ipv4UnicastAddpathNlri = (*self).try_into().unwrap();
                        nlri.compose(target)
                    }
                    None => {
                        let nlri: Ipv4UnicastNlri = (*self).try_into().unwrap();
                        nlri.compose(target)
                    }
                }
            },
            AfiSafiType::Ipv6Unicast => {
                match self.path_id {
                    Some(path_id) => {
                        let nlri: Ipv6UnicastAddpathNlri = (*self).try_into().unwrap();
                        nlri.compose(target)
                    }
                    None => {
                        let nlri: Ipv6UnicastNlri = (*self).try_into().unwrap();
                        nlri.compose(target)
                    }
                }
            },
            AfiSafiType::Ipv4Multicast => {
                match self.path_id {
                    Some(path_id) => {
                        let nlri: Ipv4MulticastAddpathNlri = (*self).try_into().unwrap();
                        nlri.compose(target)
                    }
                    None => {
                        let nlri: Ipv4MulticastNlri = (*self).try_into().unwrap();
                        nlri.compose(target)
                    }
                }
            },
            AfiSafiType::Ipv6Multicast => {
                match self.path_id {
                    Some(path_id) => {
                        let nlri: Ipv6MulticastAddpathNlri = (*self).try_into().unwrap();
                        nlri.compose(target)
                    }
                    None => {
                        let nlri: Ipv6MulticastNlri = (*self).try_into().unwrap();
                        nlri.compose(target)
                    }
                }
            },
            _ => {
                panic!("not a basic nlri");
            }
        }

        
    }
}


//------------ Ipv4 conversions ----------------------------------------------

impl From<Ipv4UnicastNlri> for BasicNlri {
    fn from(value: Ipv4UnicastNlri) -> Self {
        Self {
            ty: AfiSafiType::Ipv4Unicast,
            prefix: value.prefix(),
            path_id: None,
        }
    }
}

impl TryFrom<BasicNlri> for Ipv4UnicastNlri {
    type Error = &'static str;

    fn try_from(value: BasicNlri) -> Result<Self, Self::Error> {
        Ipv4UnicastNlri::try_from(value.prefix)
    }
}

impl From<Ipv4UnicastAddpathNlri> for BasicNlri {
    fn from(value: Ipv4UnicastAddpathNlri) -> Self {
        Self {
            ty: AfiSafiType::Ipv4Unicast,
            prefix: value.prefix(),
            path_id: Some(Addpath::path_id(&value)),
        }
    }
}

impl TryFrom<BasicNlri> for Ipv4UnicastAddpathNlri {
    type Error = &'static str;

    fn try_from(value: BasicNlri) -> Result<Self, Self::Error> {
        Ipv4UnicastAddpathNlri::try_from(
            (value.prefix, value.path_id.unwrap())
        )
    }
}

impl From<Ipv4MulticastNlri> for BasicNlri {
    fn from(value: Ipv4MulticastNlri) -> Self {
        Self {
            ty: AfiSafiType::Ipv4Unicast,
            prefix: value.prefix(),
            path_id: None,
        }
    }
}

impl TryFrom<BasicNlri> for Ipv4MulticastNlri {
    type Error = &'static str;

    fn try_from(value: BasicNlri) -> Result<Self, Self::Error> {
        Ipv4MulticastNlri::try_from(value.prefix)
    }
}

impl From<Ipv4MulticastAddpathNlri> for BasicNlri {
    fn from(value: Ipv4MulticastAddpathNlri) -> Self {
        Self {
            ty: AfiSafiType::Ipv4Unicast,
            prefix: value.prefix(),
            path_id: Some(Addpath::path_id(&value)),
        }
    }
}

impl TryFrom<BasicNlri> for Ipv4MulticastAddpathNlri {
    type Error = &'static str;

    fn try_from(value: BasicNlri) -> Result<Self, Self::Error> {
        Ipv4MulticastAddpathNlri::try_from(
            (value.prefix, value.path_id.unwrap())
        )
    }
}

//------------ Ipv6 conversions ----------------------------------------------

impl From<Ipv6UnicastNlri> for BasicNlri {
    fn from(value: Ipv6UnicastNlri) -> Self {
        Self {
            ty: AfiSafiType::Ipv6Unicast,
            prefix: value.prefix(),
            path_id: None,
        }
    }
}

impl TryFrom<BasicNlri> for Ipv6UnicastNlri {
    type Error = &'static str;

    fn try_from(value: BasicNlri) -> Result<Self, Self::Error> {
        Ipv6UnicastNlri::try_from(value.prefix)
    }
}

impl From<Ipv6UnicastAddpathNlri> for BasicNlri {
    fn from(value: Ipv6UnicastAddpathNlri) -> Self {
        Self {
            ty: AfiSafiType::Ipv6Unicast,
            prefix: value.prefix(),
            path_id: Some(Addpath::path_id(&value)),
        }
    }
}

impl TryFrom<BasicNlri> for Ipv6UnicastAddpathNlri {
    type Error = &'static str;

    fn try_from(value: BasicNlri) -> Result<Self, Self::Error> {
        Ipv6UnicastAddpathNlri::try_from(
            (value.prefix, value.path_id.unwrap())
        )
    }
}

impl From<Ipv6MulticastNlri> for BasicNlri {
    fn from(value: Ipv6MulticastNlri) -> Self {
        Self {
            ty: AfiSafiType::Ipv6Unicast,
            prefix: value.prefix(),
            path_id: None,
        }
    }
}

impl TryFrom<BasicNlri> for Ipv6MulticastNlri {
    type Error = &'static str;

    fn try_from(value: BasicNlri) -> Result<Self, Self::Error> {
        Ipv6MulticastNlri::try_from(value.prefix)
    }
}

impl From<Ipv6MulticastAddpathNlri> for BasicNlri {
    fn from(value: Ipv6MulticastAddpathNlri) -> Self {
        Self {
            ty: AfiSafiType::Ipv6Unicast,
            prefix: value.prefix(),
            path_id: Some(Addpath::path_id(&value)),
        }
    }
}

impl TryFrom<BasicNlri> for Ipv6MulticastAddpathNlri {
    type Error = &'static str;

    fn try_from(value: BasicNlri) -> Result<Self, Self::Error> {
        Ipv6MulticastAddpathNlri::try_from(
            (value.prefix, value.path_id.unwrap())
        )
    }
}

impl<O> TryFrom<Nlri<O>> for BasicNlri {
    type Error = &'static str;

    fn try_from(n: Nlri<O>) -> Result<Self, Self::Error> {
        match n {
            Nlri::Ipv4Unicast(_) => todo!(),
            Nlri::Ipv4UnicastAddpath(_) => todo!(),
            Nlri::Ipv4Multicast(_) => todo!(),
            Nlri::Ipv4MulticastAddpath(_) => todo!(),
            Nlri::Ipv6Unicast(_) => todo!(),
            Nlri::Ipv6UnicastAddpath(_) => todo!(),
            Nlri::Ipv6Multicast(_) => todo!(),
            Nlri::Ipv6MulticastAddpath(_) => todo!(),
            _ => Err("NLRI not basic"),
        }
    }
}

/// Creates a Vec with all of the single `NLRIs` for all `NLRI` and converts
/// all the values in `T`.
///
/// The type of `NLRI` is specified with the `AF` type argument. If the
/// specified `NLRI` type is not present, it will return an empty `Vec`. The
/// type `T` should be able to convert all the different `NLRI` types in the
/// Vec to `T`.
pub fn explode_announcements<
    'a,
    P: Octets,
    O: Octets
        + Octets<Range<'a> = O>
        + 'a
        + Clone
        + std::fmt::Debug
        + std::hash::Hash,
    T: From<RouteWorkshop<BasicNlri>>
        + From<RouteWorkshop<Ipv4FlowSpecNlri<O>>>
        + From<RouteWorkshop<Ipv6FlowSpecNlri<O>>>,
>(
    update: &'a UpdateMessage<O>,
) -> Result<Vec<T>, ParseError>
where
    std::vec::Vec<u8>: std::convert::From<O>,
{
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
                        T::from(RouteWorkshop::from_pa_map(
                            BasicNlri::from(a.unwrap()),
                            pa_map.clone(),
                        ))
                    })
                    .collect::<Vec<T>>(),
                );
            }
            NlriType::Ipv4UnicastAddpath => {
                let iter = update
                    .typed_announcements::<_, Ipv4UnicastAddpathNlri>()
                    .unwrap()
                    .unwrap();

                res.extend(
                    iter.map(|a| {
                        T::from(RouteWorkshop::from_pa_map(
                            BasicNlri::from(a.unwrap()),
                            pa_map.clone(),
                        ))
                    })
                    .collect::<Vec<T>>(),
                );
            }
            NlriType::Ipv6Unicast => {
                let iter = update
                    .typed_announcements::<_, Ipv6UnicastNlri>()
                    .unwrap()
                    .unwrap();
                res.extend(
                    iter.map(|a| {
                        T::from(RouteWorkshop::from_pa_map(
                            BasicNlri::from(a.unwrap()),
                            pa_map.clone(),
                        ))
                    })
                    .collect::<Vec<T>>(),
                );
            }
            NlriType::Ipv6UnicastAddpath => {
                let iter = update
                    .typed_announcements::<_, Ipv6UnicastAddpathNlri>()
                    .unwrap()
                    .unwrap();
                res.extend(
                    iter.map(|a| {
                        T::from(RouteWorkshop::from_pa_map(
                            BasicNlri::from(a.unwrap()),
                            pa_map.clone(),
                        ))
                    })
                    .collect::<Vec<T>>(),
                );
            }
            NlriType::Ipv4Multicast => {
                let iter = update
                    .typed_announcements::<_, Ipv4MulticastNlri>()
                    .unwrap()
                    .unwrap();
                res.extend(
                    iter.map(|a| {
                        T::from(RouteWorkshop::from_pa_map(
                            BasicNlri::from(a.unwrap()),
                            pa_map.clone(),
                        ))
                    })
                    .collect::<Vec<T>>(),
                );
            }
            NlriType::Ipv4MulticastAddpath => {
                let iter = update
                    .typed_announcements::<_, Ipv4MulticastAddpathNlri>()
                    .unwrap()
                    .unwrap();
                res.extend(
                    iter.map(|a| {
                        T::from(RouteWorkshop::from_pa_map(
                            BasicNlri::from(a.unwrap()),
                            pa_map.clone(),
                        ))
                    })
                    .collect::<Vec<T>>(),
                );
            }
            NlriType::Ipv6Multicast => {
                let iter = update
                    .typed_announcements::<_, Ipv6MulticastNlri>()
                    .unwrap()
                    .unwrap();
                res.extend(
                    iter.map(|a| {
                        T::from(RouteWorkshop::from_pa_map(
                            BasicNlri::from(a.unwrap()),
                            pa_map.clone(),
                        ))
                    })
                    .collect::<Vec<T>>(),
                );
            }
            NlriType::Ipv6MulticastAddpath => {
                let iter = update
                    .typed_announcements::<_, Ipv6MulticastAddpathNlri>()
                    .unwrap()
                    .unwrap();
                res.extend(
                    iter.map(|a| {
                        T::from(RouteWorkshop::from_pa_map(
                            BasicNlri::from(a.unwrap()),
                            pa_map.clone(),
                        ))
                    })
                    .collect::<Vec<T>>(),
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
                    .typed_announcements::<_, Ipv4FlowSpecNlri<O>>()
                    .unwrap()
                    .unwrap();
                res.extend(
                    iter.map(|a| {
                        T::from(RouteWorkshop::from_pa_map(
                            a.unwrap(),
                            pa_map.clone(),
                        ))
                    })
                    .collect::<Vec<T>>(),
                );
            }
            NlriType::Ipv4FlowSpecAddpath => todo!(),
            NlriType::Ipv6FlowSpec => {
                let iter = update
                    .typed_announcements::<_, Ipv6FlowSpecNlri<O>>()
                    .unwrap()
                    .unwrap();
                res.extend(
                    iter.map(|a| {
                        T::from(RouteWorkshop::from_pa_map(
                            a.unwrap(),
                            pa_map.clone(),
                        ))
                    })
                    .collect::<Vec<T>>(),
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
        use routecore::bgp::message::SessionConfig;

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

        let announces = pdu.announcement_fams();
        let rws = explode_announcements::<'_, bytes::Bytes, _, TypeValue>(&pdu);

        // let res = pdu_into_typed_rws::<_, RouteWorkshop<BasicNlri>, _, Ipv6UnicastNlri>(&pdu);
        // for rws in &res {
        //     println!("{}", rws.nlri());
        // }
        // assert_eq!(res.len(), 5);

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
