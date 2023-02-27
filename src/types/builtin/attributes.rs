use routecore::{
    asn::{
        AsPath, Asn, MaterializedPathSegment,
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

// Wrapper for all different values of BGP Path Attributes (as listed in
// https://www.iana.org/assignments/bgp-parameters/bgp-parameters.xhtml)
// that can be encountered in a BGP Update Message.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AttrChangeSet {
    pub as_path: ChangedOption<AsPath<Vec<MaterializedPathSegment>>>,
    pub origin_type: ChangedOption<OriginType>,
    pub next_hop: ChangedOption<NextHop>,
    pub multi_exit_discriminator: ChangedOption<MultiExitDisc>,
    pub local_pref: ChangedOption<LocalPref>,
    pub atomic_aggregate: ChangedOption<bool>,
    pub aggregator: ChangedOption<Aggregator>,
    pub communities: ChangedOption<Vec<Community>>,
    // mp_reach_nlri: Vec<Prefix>,
    // mp_unreach_nlri: Vec<Prefix>,
    pub originator_id: ChangedOption<Todo>,
    pub cluster_list: ChangedOption<Todo>,
    pub extended_communities: ChangedOption<Vec<ExtendedCommunity>>,
    pub as4_path: ChangedOption<AsPath<Vec<Asn>>>,
    pub as4_aggregator: ChangedOption<Todo>,
    pub connector: ChangedOption<Todo>, // Connector,
    pub as_path_limit: ChangedOption<(u8, u32)>,
    pub pmsi_tunnel: ChangedOption<Todo>, // PmsiTunnel,
    pub ipv6_extended_communities: ChangedOption<Vec<Ipv6ExtendedCommunity>>,
    pub large_communities: ChangedOption<Vec<LargeCommunity>>,
    pub bgpsec_as_path: ChangedOption<Todo>, // BgpsecAsPath,
    pub attr_set: ChangedOption<Todo>,       // AttrSet,
    pub rsrvd_development: ChangedOption<Todo>, // RsrvdDevelopment,
}


//------------ ChangedOption ------------------------------------------------

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
pub struct Todo;

impl ChangedOption<AsPath<Vec<MaterializedPathSegment>>> {
    pub fn new(as_path: AsPath<Vec<Asn>>) -> Self {
        Self {
            value: Some(AsPath {
                segments: as_path
                    .segments
                    .iter()
                    .map(|p| MaterializedPathSegment::from(*p))
                    .collect(),
            }),
            changed: false,
        }
    }

    pub fn get(&self, pos: u8) -> Option<&MaterializedPathSegment> {
        self.value.as_ref().and_then(|p| p.get(pos))
    }

    // Undoes all previous modifications in this change set!
    pub fn replace_with(
        &mut self,
        as_path: AsPath<Vec<Asn>>,
    ) -> Result<(), VmError> {
        self.value = Some(as_path.into());
        self.changed = true;
        Ok(())
    }

    pub fn append(&mut self, asn: Asn) -> Result<(), VmError> {
        self.value.as_mut().map(|p| p.append(vec![asn]));
        self.changed = true;

        Ok(())
    }

    pub fn prepend(&mut self, asn: Asn) -> Result<(), VmError> {
        self.value
            .as_mut()
            .map(|p| p.prepend(vec![asn]));
        self.changed = true;

        Ok(())
    }

    pub fn insert(&mut self, pos: u8, asn: Asn) -> Result<(), VmError> {
        self.value
            .as_mut()
            .map(|p| p.insert(pos, vec![asn]));
        self.changed = true;

        Ok(())
    }

    pub fn len(&self) -> Option<u8> {
        self.value.as_ref().map(|p| p.len())
    }
}


//------------ Scalar Trait -------------------------------------------------

pub trait ScalarValue: Copy {}

impl ScalarValue for NextHop {}
impl ScalarValue for OriginType {}
impl ScalarValue for bool {}
impl ScalarValue for MultiExitDisc {}
impl ScalarValue for LocalPref {}
impl ScalarValue for Aggregator {}
impl ScalarValue for (u8, u32) {}

impl<T: ScalarValue> ChangedOption<T> {
    pub fn get(&self) -> Option<T> {
        self.value
    }

    // Sets the scalar and returns the current value
    pub fn set(&mut self, next_hop: T) -> Option<T> {
        let val = &mut Some(next_hop);
        std::mem::swap(&mut self.value, val);
        self.changed = true;
        *val
    }

    // Clears and sets the changed bool, so this is
    // a deliberate wipe of the value.
    pub fn clear(&mut self) {
        *self = ChangedOption::cleared()
    }
}
