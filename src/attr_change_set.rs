//------------ Route Status -------------------------------------------------

use routecore::asn::{LongSegmentError, OwnedPathSegment};
use std::marker::PhantomData;
use std::ops::Index;

use crate::types::builtin::{
    AsPath, Asn, AtomicAggregator, BuiltinTypeValue, Community, LocalPref,
    MultiExitDisc, NextHop, OriginType, Prefix,
};
use crate::types::typevalue::TypeValue;

// The values that live in a BGP Update message can be either Scalars or
// Vectors. The two traits, ScalarValue and VectorValue, supply the methods
// to modify and inspect them and creating new BGP Update messages with them

//------------ VectorValue Trait --------------------------------------------
pub trait VectorValue: Index<usize> + From<Vec<Self::ReadItem>>
where
    Self::ReadItem: Sized + Clone,
{
    type ReadItem;
    type WriteItem;

    fn prepend_vec(
        &mut self,
        vector: Vec<Self::WriteItem>,
    ) -> Result<(), LongSegmentError>;
    fn append_vec(
        &mut self,
        vector: Vec<Self::WriteItem>,
    ) -> Result<(), LongSegmentError>;
    fn insert_vec(
        &mut self,
        pos: u8,
        vector: Vec<Self::WriteItem>,
    ) -> Result<(), LongSegmentError>;
    fn vec_len(&self) -> Option<usize>;
    // fn get(&self, pos: u8) -> Option<&Self::Item>;
    fn vec_is_empty(&self) -> bool;
    fn into_vec(self) -> Vec<Self::ReadItem>;
}

//------------ ScalarValue Trait --------------------------------------------

pub trait ScalarValue: Clone + Into<TypeValue> {}

impl ScalarValue for NextHop {}
impl ScalarValue for OriginType {}
impl ScalarValue for bool {}
impl ScalarValue for MultiExitDisc {}
impl ScalarValue for LocalPref {}
impl ScalarValue for AtomicAggregator {}
impl ScalarValue for Community {}
impl ScalarValue for Prefix {}
// impl ScalarValue for (u8, u32) {}

//------------ Attributes Change Set ----------------------------------------

// A attributes Change Set allows a user to create a set of changes to an
// existing (raw) BGP Update message.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AttrChangeSet {
    pub prefix: ReadOnlyScalarOption<Prefix>, // Read-only prefix typevalue, for referencing it.
    pub as_path: VectorOption<AsPath>,
    pub origin_type: ScalarOption<OriginType>,
    pub next_hop: ScalarOption<NextHop>,
    pub multi_exit_discriminator: ScalarOption<MultiExitDisc>,
    pub local_pref: ScalarOption<LocalPref>,
    pub atomic_aggregate: ScalarOption<bool>,
    pub aggregator: ScalarOption<AtomicAggregator>,
    pub communities: VectorOption<Vec<Community>>,
    // mp_reach_nlri: Vec<Prefix>,
    // mp_unreach_nlri: Vec<Prefix>,
    pub originator_id: Todo,
    pub cluster_list: Todo,
    pub extended_communities: Todo,
    pub as4_path: VectorOption<AsPath>,
    pub as4_aggregator: Todo,
    pub connector: Todo, // Connector,
    pub as_path_limit: Todo,
    pub pmsi_tunnel: Todo, // PmsiTunnel,
    pub ipv6_extended_communities: Todo,
    pub large_communities: Todo,
    pub bgpsec_as_path: Todo,    // BgpsecAsPath,
    pub attr_set: Todo,          // AttrSet,
    pub rsrvd_development: Todo, // RsrvdDevelopment,
}

//------------ ScalarOption ------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ScalarOption<T: ScalarValue> {
    pub value: Option<TypeValue>,
    pub changed: bool,
    _pd: PhantomData<T>,
}

impl<T: ScalarValue> ScalarOption<T> {
    pub fn into_opt(self) -> Option<TypeValue>
    where
        T: Copy,
    {
        self.value
    }

    pub fn as_ref(&self) -> Option<&TypeValue> {
        self.value.as_ref()
    }

    pub fn is_changed(&self) -> bool {
        self.changed
    }

    pub fn new() -> ScalarOption<TypeValue> {
        ScalarOption {
            value: None,
            changed: false,
            _pd: PhantomData,
        }
    }

    pub fn set_cleared() -> ScalarOption<TypeValue> {
        ScalarOption {
            value: None,
            changed: true,
            _pd: PhantomData,
        }
    }

    pub fn set<S: Into<TypeValue>>(&mut self, value: S) -> Option<TypeValue> {
        let val = &mut Some(value.into());
        std::mem::swap(&mut self.value, val);
        self.changed = true;
        val.clone()
    }
}

impl<S1: Into<TypeValue>, S2: ScalarValue + Into<TypeValue>> From<Option<S1>>
    for ScalarOption<S2>
{
    fn from(value: Option<S1>) -> Self {
        match value {
            Some(v) => ScalarOption {
                value: Some(v.into()),
                changed: false,
                _pd: PhantomData,
            },
            None => ScalarOption {
                value: None,
                changed: false,
                _pd: PhantomData,
            },
        }
    }
}

//------------ ReadOnlyScalarOption -----------------------------------------

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ReadOnlyScalarOption<T: ScalarValue + Into<TypeValue>> {
    pub value: Option<TypeValue>,
    _pd: PhantomData<T>,
}

impl<T: ScalarValue + Into<TypeValue>> ReadOnlyScalarOption<T> {
    pub fn new(value: TypeValue) -> Self {
        Self {
            value: Some(value),
            _pd: PhantomData,
        }
    }

    pub fn as_ref(&self) -> Option<&TypeValue> {
        self.value.as_ref()
    }
}

//------------ TodoOption ---------------------------------------------------

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct Todo;

//------------ VectorOption -------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VectorOption<V: VectorValue + Into<TypeValue>> {
    pub value: Option<TypeValue>,
    pub changed: bool,
    _pd: PhantomData<V>,
}

impl<V: VectorValue + Into<TypeValue>> VectorOption<V> {
    pub fn new(vector: V) -> Self {
        Self {
            value: Some(vector.into()),
            changed: false,
            _pd: PhantomData,
        }
    }

    pub fn get_from_vec(&self, pos: u8) -> Option<TypeValue> {
        self.value.as_ref().map(|c| c[pos as usize].clone())
    }

    pub fn replace(&mut self, vector: V) -> Result<(), LongSegmentError> {
        self.value = Some(vector.into());
        self.changed = true;
        Ok(())
    }

    pub fn prepend(&mut self, value: V) -> Result<(), LongSegmentError> {
        if let Some(tv) = self.value.as_mut() {
            match tv {
                TypeValue::List(list)
                | TypeValue::Builtin(BuiltinTypeValue::Communities(list)) => {
                    list.prepend_vec(vec![value.into()])
                        .map_err(|_| LongSegmentError)?;
                }
                TypeValue::Builtin(BuiltinTypeValue::AsPath(as_path)) => {
                    let asn_vec = if let TypeValue::Builtin(
                        BuiltinTypeValue::Asn(Asn(asn)),
                    ) = value.into()
                    {
                        vec![asn]
                    } else {
                        vec![]
                    };
                    as_path
                        .prepend_vec(asn_vec)
                        .map_err(|_| LongSegmentError)?;
                }
                _ => {}
            }
        }
        self.changed = true;

        Ok(())
    }

    pub fn append(&mut self, value: V) -> Result<(), LongSegmentError> {
        if let Some(tv) = self.value.as_mut() {
            match tv {
                TypeValue::List(list)
                | TypeValue::Builtin(BuiltinTypeValue::Communities(list)) => {
                    list.append_vec(vec![value.into()])
                        .map_err(|_| LongSegmentError)?;
                }
                TypeValue::Builtin(BuiltinTypeValue::AsPath(as_path)) => {
                    let asn_vec = if let TypeValue::Builtin(
                        BuiltinTypeValue::Asn(Asn(asn)),
                    ) = value.into()
                    {
                        vec![asn]
                    } else {
                        vec![]
                    };
                    as_path
                        .append_vec(asn_vec)
                        .map_err(|_| LongSegmentError)?;
                }
                _ => {}
            }
        }
        self.changed = true;

        Ok(())
    }

    pub fn insert(
        &mut self,
        pos: u8,
        value: V,
    ) -> Result<(), LongSegmentError> {
        if let Some(tv) = self.value.as_mut() {
            match tv {
                TypeValue::List(list)
                | TypeValue::Builtin(BuiltinTypeValue::Communities(list)) => {
                    list.insert_vec(pos as usize, vec![value.into()])
                        .map_err(|_| LongSegmentError)?;
                }
                TypeValue::Builtin(BuiltinTypeValue::AsPath(as_path)) => {
                    let asn_vec = if let TypeValue::Builtin(
                        BuiltinTypeValue::Asn(Asn(asn)),
                    ) = value.into()
                    {
                        vec![asn]
                    } else {
                        vec![]
                    };
                    as_path
                        .insert_vec(pos, asn_vec)
                        .map_err(|_| LongSegmentError)?;
                }
                _ => {}
            }
        }
        self.changed = true;

        Ok(())
    }

    pub fn len(&self) -> Option<usize> {
        self.value.as_ref().and_then(|tv| match tv {
            TypeValue::List(list)
            | TypeValue::Builtin(BuiltinTypeValue::Communities(list)) => {
                Some(list.len())
            }
            TypeValue::Builtin(BuiltinTypeValue::AsPath(as_path)) => {
                as_path.vec_len()
            }
            _ => None,
        })
    }

    pub fn is_empty(&self) -> bool {
        self.value.as_ref().map_or_else(
            || true,
            |tv| match tv {
                TypeValue::List(list)
                | TypeValue::Builtin(BuiltinTypeValue::Communities(list)) => {
                    list.is_empty()
                }
                TypeValue::Builtin(BuiltinTypeValue::AsPath(as_path)) => {
                    as_path.vec_is_empty()
                }
                _ => true,
            },
        )
    }
}

//------------ VectorValue --------------------------------------------------

impl<V1: Into<TypeValue>, V2: VectorValue + Into<TypeValue>> From<Option<V1>>
    for VectorOption<V2>
{
    fn from(value: Option<V1>) -> Self {
        match value {
            Some(v) => VectorOption {
                value: Some(v.into()),
                changed: false,
                _pd: PhantomData,
            },
            None => VectorOption {
                value: None,
                changed: false,
                _pd: PhantomData,
            },
        }
    }
}

impl From<Vec<OwnedPathSegment>> for AsPath {
    fn from(value: Vec<OwnedPathSegment>) -> Self {
        todo!()
    }
}

impl TryFrom<Vec<routecore::asn::Asn>> for AsPath {
    type Error = LongSegmentError;

    fn try_from(
        value: Vec<routecore::asn::Asn>,
    ) -> Result<Self, LongSegmentError> {
        routecore::asn::AsPath::try_from(value.as_slice()).map(AsPath)
    }
}

impl std::ops::Index<usize> for AsPath {
    type Output = OwnedPathSegment;

    fn index(&self, index: usize) -> &Self::Output {
        unimplemented!()
    }
}

impl<T: ScalarValue> VectorValue for Vec<T> {
    type WriteItem = T;
    type ReadItem = T;

    fn prepend_vec(
        &mut self,
        vector: Vec<Self::WriteItem>,
    ) -> Result<(), LongSegmentError> {
        let mut new_vec = Vec::with_capacity(self.len() + vector.len());
        std::mem::swap(&mut new_vec, self);
        self.extend(vector);
        self.extend(new_vec);
        Ok(())
    }

    fn append_vec(
        &mut self,
        vector: Vec<Self::WriteItem>,
    ) -> Result<(), LongSegmentError> {
        self.extend(vector);
        Ok(())
    }

    fn insert_vec(
        &mut self,
        pos: u8,
        vector: Vec<Self::WriteItem>,
    ) -> Result<(), LongSegmentError> {
        let mut new_vec = Vec::with_capacity(self.len() + vector.len());
        std::mem::swap(&mut new_vec, self);
        self.extend_from_slice(&new_vec[..pos as usize]);
        self.extend(vector);
        self.extend_from_slice(&new_vec[pos as usize..]);
        Ok(())
    }

    fn vec_len(&self) -> Option<usize> {
        Some(self.len())
    }

    fn vec_is_empty(&self) -> bool {
        self.is_empty()
    }

    fn into_vec(self) -> Vec<T> {
        self
    }
}

// Status is piece of metadata that writes some (hopefully) relevant state of
// per-peer BGP session into every route. The goal is to be able to enable
// the logic in `rib-units` to decide whether routes should be send to its
// output and to be able output this information to API clients, without
// having to go back to the units that keep the per-peer session state.
#[derive(Debug, Eq, PartialEq, Copy, Clone, Default)]
pub enum RouteStatus {
    // Between start and EOR on a BGP peer-session
    InConvergence,
    // After EOR for a BGP peer-session, either `Graceful Restart` or EOR
    UpToDate,
    // After hold-timer expiry
    Stale,
    // After the request for a Route Refresh to a peer and the reception of a
    // new route
    StartOfRouteRefresh,
    // After the reception of a withdrawal
    Withdrawn,
    // Status not relevant, e.g. a RIB that holds archived routes.
    #[default]
    Empty,
}

impl ScalarValue for RouteStatus {}

impl std::fmt::Display for RouteStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RouteStatus::InConvergence => write!(f, "in convergence"),
            RouteStatus::UpToDate => write!(f, "up to date"),
            RouteStatus::Stale => write!(f, "stale"),
            RouteStatus::StartOfRouteRefresh => {
                write!(f, "start of route refresh")
            }
            RouteStatus::Withdrawn => write!(f, "withdrawn"),
            RouteStatus::Empty => write!(f, "empty"),
        }
    }
}

impl From<TypeValue> for RouteStatus {
    fn from(value: TypeValue) -> Self {
        if let TypeValue::Builtin(BuiltinTypeValue::RouteStatus(value)) =
            value
        {
            value
        } else {
            panic!("invalid something");
        }
    }
}
