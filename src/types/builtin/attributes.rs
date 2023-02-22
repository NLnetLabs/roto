use std::rc::Rc;

use routecore::{
    asn::{
        encode_sentinel, AsPath, AsPathBuilder, Asn, LongSegmentError,
        MaterializedPathSegment, PathSegment, SegmentType,
    },
    bgp::communities::{
        Community, ExtendedCommunity, Ipv6ExtendedCommunity, LargeCommunity,
    },
};

use routecore::bgp::message::update::{
    Aggregator, LocalPref, MultiExitDisc, NextHop, OriginType,
};

use crate::vm::VmError;

//------------ Attributes Change Set ----------------------------------------

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ChangedOption<T> {
    pub(crate) value: Option<T>,
    pub changed: bool,
}

impl<T> ChangedOption<T> {
    pub fn into_opt(self) -> Option<T>
    where
        T: Copy,
    {
        self.value
    }

    pub fn as_ref(&self) -> Option<&T> {
        self.value.as_ref()
    }

    pub fn changed(&self) -> bool {
        self.changed
    }

    pub fn empty() -> ChangedOption<T> {
        ChangedOption {
            value: None,
            changed: false,
        }
    }

    pub fn cleared() -> ChangedOption<T> {
        ChangedOption {
            value: None,
            changed: true,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct Todo {}

// Wrapper for all different values of BGP Path Attributes (as listed in
// https://www.iana.org/assignments/bgp-parameters/bgp-parameters.xhtml)
// that can be encountered in a BGP Update Message.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AttrChangeSet {
    pub as_path: AsPathModifier,
    pub origin_type: ChangedOption<OriginType>,
    pub next_hop: ChangedOption<NextHop>,
    pub multi_exit_discriminator: ChangedOption<MultiExitDisc>,
    pub local_pref: ChangedOption<LocalPref>,
    pub atomic_aggregate: ChangedOption<bool>,
    pub aggregator: ChangedOption<Aggregator>,
    pub communities: ChangedOption<Rc<[Community]>>,
    // mp_reach_nlri: Vec<Prefix>,
    // mp_unreach_nlri: Vec<Prefix>,
    pub originator_id: ChangedOption<Todo>,
    pub cluster_list: ChangedOption<Todo>,
    pub extended_communities: ChangedOption<Rc<[ExtendedCommunity]>>,
    pub as4_path: ChangedOption<AsPath<Rc<[Asn]>>>,
    pub as4_aggregator: ChangedOption<Todo>,
    pub connector: ChangedOption<Todo>, // Connector,
    pub as_path_limit: ChangedOption<(u8, u32)>,
    pub pmsi_tunnel: ChangedOption<Todo>, // PmsiTunnel,
    pub ipv6_extended_communities: ChangedOption<Rc<[Ipv6ExtendedCommunity]>>,
    pub large_communities: ChangedOption<Rc<[LargeCommunity]>>,
    pub bgpsec_as_path: ChangedOption<Todo>, // BgpsecAsPath,
    pub attr_set: ChangedOption<Todo>,       // AttrSet,
    pub rsrvd_development: ChangedOption<Todo>, // RsrvdDevelopment,
}

impl AttrChangeSet {
    pub fn copy_change_set(&self) -> Self {
        Self {
            as_path: self.as_path.clone(),
            origin_type: self.origin_type,
            next_hop: self.next_hop,
            multi_exit_discriminator: self.multi_exit_discriminator,
            local_pref: self.local_pref,
            atomic_aggregate: self.atomic_aggregate,
            aggregator: self.aggregator,
            communities: self.communities.clone(),
            originator_id: self.originator_id,
            cluster_list: self.cluster_list,
            extended_communities: self.extended_communities.clone(),
            as4_path: self.as4_path.clone(),
            as4_aggregator: self.as4_aggregator,
            connector: self.connector,
            as_path_limit: self.as_path_limit,
            pmsi_tunnel: self.pmsi_tunnel,
            ipv6_extended_communities: self.ipv6_extended_communities.clone(),
            large_communities: self.large_communities.clone(),
            bgpsec_as_path: self.bgpsec_as_path,
            attr_set: self.attr_set,
            rsrvd_development: self.rsrvd_development,
        }
    }

    pub fn get_as_path(&self) -> Option<AsPath<Rc<[Asn]>>> {
        self.as_path
            .as_path
            .as_ref()
            .map(AsPath::for_segments_rc_slice)
    }

    pub fn set_as_path(
        &mut self,
        as_path: AsPath<Vec<Asn>>,
    ) -> Result<(), VmError> {
        self.as_path.as_path.changed = true;
        self.as_path.replace_with(as_path)
    }

    pub fn prepend_to_as_path(&mut self, asn: Asn) -> Result<(), VmError> {
        self.as_path.as_path.changed = true;
        self.as_path.prepend(asn)
    }

    pub fn append_to_as_path(&mut self, asn: Asn) -> Result<(), VmError> {
        self.as_path.as_path.changed = true;
        self.as_path.append(asn)
    }

    pub fn insert_asn_into_as_path(
        &mut self,
        position: u8,
        asn: Asn,
    ) -> Result<(), VmError> {
        self.as_path.as_path.changed = true;
        self.as_path.insert(position, asn)
    }

    pub fn materialize_as_path(
        &mut self,
    ) -> Result<AsPath<Vec<MaterializedPathSegment>>, VmError> {
        self.as_path.materialize()
    }

    pub fn get_origin_type(&self) -> Option<OriginType> {
        self.origin_type.into_opt()
    }

    pub fn get_next_hop(&self) -> Option<NextHop> {
        self.next_hop.into_opt()
    }

    pub fn set_next_hop(&mut self, next_hop: NextHop) {
        self.next_hop = ChangedOption {
            value: Some(next_hop),
            changed: true,
        }
    }

    pub fn clear_next_hop(&mut self) {
        self.next_hop = ChangedOption::cleared()
    }
}

//------------ AsPathModifier -----------------------------------------------

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AsPathModifier {
    as_path: ChangedOption<AsPath<Rc<[Asn]>>>,
    modifications: Vec<AsPathAction>,
    mod_len: usize, // length of the path after all mods are applied.
}

impl AsPathModifier {
    pub fn new(as_path: AsPath<Rc<[Asn]>>) -> Self {
        Self {
            mod_len: as_path.segments.len(),
            as_path: ChangedOption {
                value: Some(as_path),
                changed: false,
            },
            modifications: vec![],
        }
    }

    // Undoes all previous modifications in this change set!
    pub fn replace_with(
        &mut self,
        as_path: AsPath<Vec<Asn>>,
    ) -> Result<(), VmError> {
        let len = as_path.segments.len();
        if len >= 255 {
            return Err(VmError::AsPathTooLong);
        }
        self.mod_len = as_path.segments.len();
        self.modifications = vec![AsPathAction::Replace(as_path)];
        Ok(())
    }

    pub fn append(&mut self, asn: Asn) -> Result<(), VmError> {
        if self.mod_len >= 255 {
            return Err(VmError::AsPathTooLong);
        }
        self.modifications.push(AsPathAction::Append(asn));
        self.mod_len += 1;
        Ok(())
    }

    pub fn prepend(&mut self, asn: Asn) -> Result<(), VmError> {
        if self.mod_len >= 255 {
            return Err(VmError::AsPathTooLong);
        }
        self.modifications.push(AsPathAction::Prepend(asn));
        self.mod_len += 1;
        Ok(())
    }

    pub fn insert(&mut self, position: u8, asn: Asn) -> Result<(), VmError> {
        if self.mod_len <= position as usize {
            self.modifications
                .push(AsPathAction::Insert((position, asn)));
        } else {
            return Err(VmError::AsPathTooLong);
        }
        Ok(())
    }

    pub fn materialize(
        &self,
    ) -> Result<AsPath<Vec<MaterializedPathSegment>>, VmError> {
        let mut as_path = AsPath {
            segments: self
                .as_path
                .value
                .as_ref()
                .unwrap()
                .iter()
                .map(|ps| ps.into())
                .collect::<Vec<MaterializedPathSegment>>(),
        };

        for modi in &self.modifications {
            match modi {
                AsPathAction::Append(asn) => {
                    as_path.segments.push(MaterializedPathSegment {
                        stype: SegmentType::Sequence,
                        elements: vec![
                            encode_sentinel(SegmentType::Sequence, 1),
                            *asn,
                        ],
                    });
                }
                AsPathAction::Prepend(asn) => {
                    let mut pre_path = AsPath {
                        segments: vec![MaterializedPathSegment {
                            stype: SegmentType::Sequence,
                            elements: vec![*asn],
                        }],
                    };
                    pre_path
                        .segments
                        .extend_from_slice(&as_path.segments.clone());
                    as_path = pre_path;
                }
                AsPathAction::Replace(replace_path) => {
                    return Ok(AsPath {
                        segments: replace_path
                            .iter()
                            .map(|ps| ps.into())
                            .collect::<Vec<_>>(),
                    })
                }
                AsPathAction::Insert((pos, asn)) => {
                    let mut pre_path =
                        (as_path.segments[0..*pos as usize]).to_vec();
                    pre_path.push(MaterializedPathSegment {
                        stype: SegmentType::Sequence,
                        elements: vec![
                            encode_sentinel(SegmentType::Sequence, 0),
                            *asn,
                        ],
                    });
                    pre_path.extend_from_slice(
                        &as_path.segments[*pos as usize..],
                    );
                }
            };
        }

        Ok(as_path)
    }

    pub fn consume_materialize(
        &mut self,
    ) -> Result<AsPath<Vec<Asn>>, VmError> {
        let mut as_path = AsPathBuilder::new();

        for modi in &mut self.modifications {
            match modi {
                AsPathAction::Append(asn) => {
                    as_path.extend_from_slice(&as_path.segments.clone())?;
                    as_path.push(*asn)?;
                }
                AsPathAction::Prepend(asn) => {
                    let mut pre_path = AsPathBuilder::new();
                    pre_path.push(*asn)?;
                    pre_path.extend_from_slice(&as_path.segments.clone())?;
                    as_path = pre_path;
                }
                AsPathAction::Replace(replace_path) => {
                    let r_path = std::mem::replace(
                        replace_path,
                        AsPath { segments: vec![] },
                    );
                    return Ok(r_path);
                }
                AsPathAction::Insert((pos, asn)) => {
                    let mut pre_path = AsPathBuilder::new();
                    pre_path.extend_from_slice(
                        &as_path.segments[..*pos as usize],
                    )?;
                    pre_path.push(*asn)?;
                }
            };
        }
        let as_path: AsPath<Vec<Asn>> = as_path.finalize();

        Ok(as_path)
    }
}

//------------ AsPathAction -------------------------------------------------

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AsPathAction {
    Append(Asn),
    Prepend(Asn),
    Insert((u8, Asn)),
    Replace(AsPath<Vec<Asn>>),
}

impl From<LongSegmentError> for VmError {
    fn from(_value: LongSegmentError) -> Self {
        VmError::AsPathTooLong
    }
}
