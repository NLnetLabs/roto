//------------ Route Status -------------------------------------------------

use routecore::asn::LongSegmentError;
use routecore::bgp::message::nlri::PathId;
use routecore::bgp::types::AfiSafi;
use serde::Serialize;
use std::marker::PhantomData;
use std::ops::Index;

use crate::ast::StringLiteral;
use crate::types::builtin::{
    AsPath, Asn, AtomicAggregate, BuiltinTypeValue, Community, IpAddress,
    LocalPref, MultiExitDisc, NextHop, OriginType, Prefix, RouteStatus,
};
use crate::types::collections::ElementTypeValue;
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
impl ScalarValue for AtomicAggregate {}
impl ScalarValue for Community {}
impl ScalarValue for Prefix {}
impl ScalarValue for RouteStatus {}
impl ScalarValue for IpAddress {}
impl ScalarValue for Asn {}
impl ScalarValue for StringLiteral {}
// impl ScalarValue for (u8, u32) {}

//------------ Attributes Change Set ----------------------------------------

// A attributes Change Set allows a user to create a set of changes to an
// existing (raw) BGP Update message.
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Hash)]
pub struct AttrChangeSet {
    #[serde(skip_serializing_if = "ReadOnlyScalarOption::is_none")]
    pub prefix: ReadOnlyScalarOption<Prefix>, // Read-only prefix typevalue, for referencing it.
    #[serde(skip_serializing_if = "VectorOption::is_none")]
    pub as_path: VectorOption<AsPath>,
    #[serde(skip_serializing_if = "ScalarOption::is_none")]
    pub origin_type: ScalarOption<OriginType>,
    #[serde(skip_serializing_if = "ScalarOption::is_none")]
    pub next_hop: ScalarOption<NextHop>,
    #[serde(skip_serializing_if = "ScalarOption::is_none")]
    pub multi_exit_discriminator: ScalarOption<MultiExitDisc>,
    #[serde(skip_serializing_if = "ScalarOption::is_none")]
    pub local_pref: ScalarOption<LocalPref>,
    #[serde(skip_serializing_if = "ScalarOption::is_none")]
    pub atomic_aggregate: ScalarOption<bool>,
    #[serde(skip_serializing_if = "ScalarOption::is_none")]
    pub aggregator: ScalarOption<AtomicAggregate>,
    #[serde(skip_serializing_if = "VectorOption::is_none")]
    pub communities: VectorOption<Vec<Community>>,
    #[serde(skip_serializing_if = "ScalarOption::is_none")]
    pub peer_ip: ScalarOption<IpAddress>,
    #[serde(skip_serializing_if = "ScalarOption::is_none")]
    pub peer_asn: ScalarOption<Asn>,
    #[serde(skip_serializing_if = "ScalarOption::is_none")]
    pub router_id: ScalarOption<StringLiteral>,
    #[serde(skip_serializing_if = "ReadOnlyScalarOption::is_none")]
    pub afi_safi: ReadOnlyScalarOption<AfiSafi>,
    #[serde(skip_serializing_if = "ScalarOption::is_none")]
    pub path_id: ScalarOption<PathId>,
    // mp_reach_nlri: Vec<Prefix>,
    // mp_unreach_nlri: Vec<Prefix>,
    #[serde(skip)]
    pub originator_id: Todo,
    #[serde(skip)]
    pub cluster_list: Todo,
    #[serde(skip)]
    pub extended_communities: Todo,
    #[serde(skip_serializing_if = "VectorOption::is_none")]
    pub as4_path: VectorOption<AsPath>,
    #[serde(skip)]
    pub as4_aggregator: Todo,
    #[serde(skip)]
    pub connector: Todo, // Connector,
    #[serde(skip)]
    pub as_path_limit: Todo,
    #[serde(skip)]
    pub pmsi_tunnel: Todo, // PmsiTunnel,
    #[serde(skip)]
    pub ipv6_extended_communities: Todo,
    #[serde(skip)]
    pub large_communities: Todo,
    #[serde(skip)]
    pub bgpsec_as_path: Todo, // BgpsecAsPath,
    #[serde(skip)]
    pub attr_set: Todo, // AttrSet,
    #[serde(skip)]
    pub rsrvd_development: Todo, // RsrvdDevelopment,
}


//------------ ScalarOption ------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
#[serde(transparent)]
pub struct ScalarOption<T: ScalarValue> {
    #[serde(skip_serializing_if = "Option::is_none")]
    value: Option<TypeValue>,
    #[serde(skip)]
    changed: bool,
    #[serde(skip)]
    _pd: PhantomData<T>,
}

impl<T: ScalarValue> ScalarOption<T> {
    pub fn is_none(&self) -> bool {
        self.value.is_none()
    }

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

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize)]
#[serde(transparent)]
pub struct ReadOnlyScalarOption<T: ScalarValue + Into<TypeValue>> {
    #[serde(skip_serializing_if = "Option::is_none")]
    value: Option<TypeValue>,
    #[serde(skip)]
    _pd: PhantomData<T>,
}

impl<T: ScalarValue + Into<TypeValue>> ReadOnlyScalarOption<T> {
    pub fn new(value: TypeValue) -> Self {
        Self {
            value: Some(value),
            _pd: PhantomData,
        }
    }

    pub fn is_none(&self) -> bool {
        self.value.is_none()
    }

    pub fn as_ref(&self) -> Option<&TypeValue> {
        self.value.as_ref()
    }
}

//------------ TodoOption ---------------------------------------------------

#[derive(Debug, PartialEq, Eq, Copy, Clone, Serialize, Hash)]
pub struct Todo;

//------------ VectorOption -------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
#[serde(transparent)]
pub struct VectorOption<V: VectorValue + Into<TypeValue>> {
    #[serde(skip_serializing_if = "Option::is_none")]
    value: Option<TypeValue>,
    #[serde(skip)]
    changed: bool,
    #[serde(skip)]
    _pd: PhantomData<V>,
}

impl<V: VectorValue + Into<TypeValue> + std::fmt::Debug> VectorOption<V> {
    pub fn new(vector: V) -> Self {
        Self {
            value: Some(vector.into()),
            changed: false,
            _pd: PhantomData,
        }
    }

    pub fn is_none(&self) -> bool {
        self.value.is_none()
    }

    pub fn into_opt(self) -> Option<TypeValue> {
        self.value
    }

    pub fn as_ref(&self) -> Option<&TypeValue> {
        self.value.as_ref()
    }

    // Only generic lists can iterator into a vector. There are specialized
    // builtin vector values, but they have their own methods for iterating
    // over them, e.g. AsPath.
    pub fn as_vec(&self) -> Vec<&ElementTypeValue> {
        if let Some(TypeValue::List(list)) = &self.value {
            list.0.iter().collect::<Vec<_>>()
        } else {
            vec![]
        }
    }

    pub fn into_vec(self) -> Vec<ElementTypeValue> {
        if let Some(TypeValue::List(list)) = self.value {
            list.0.into_iter().collect::<Vec<_>>()
        } else {
            vec![]
        }
    }

    pub fn replace(&mut self, vector: V) -> Result<(), LongSegmentError> {
        self.value = Some(vector.into());
        self.changed = true;
        Ok(())
    }

    pub fn prepend<T: Into<TypeValue> + std::fmt::Debug>(
        &mut self,
        value: T,
    ) -> Result<(), LongSegmentError> {
        if let Some(tv) = self.value.as_mut() {
            match tv {
                TypeValue::List(list) => {
                    list.prepend_vec(vec![value.into()])
                        .map_err(|_| LongSegmentError)?;
                }
                TypeValue::Builtin(BuiltinTypeValue::AsPath(as_path)) => {
                    let asn_vec = match value.into() {
                        TypeValue::Builtin(BuiltinTypeValue::Asn(asn)) => {
                            vec![asn]
                        }
                        TypeValue::Builtin(BuiltinTypeValue::Hop(hop)) => {
                            let asn: routecore::asn::Asn = hop
                                .0
                                .try_into_asn()
                                .map_err(|_| LongSegmentError)?;
                            vec![Asn(asn)]
                        }
                        TypeValue::Builtin(BuiltinTypeValue::U32(int)) => {
                            vec![Asn::from_u32(int.0)]
                        }
                        _ => {
                            return Err(LongSegmentError);
                        }
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
                TypeValue::List(list) => {
                    list.append_vec(vec![value.into()])
                        .map_err(|_| LongSegmentError)?;
                }
                TypeValue::Builtin(BuiltinTypeValue::AsPath(as_path)) => {
                    let asn_vec = if let TypeValue::Builtin(
                        BuiltinTypeValue::Asn(asn),
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
                TypeValue::List(list) => {
                    list.insert_vec(pos as usize, vec![value.into()])
                        .map_err(|_| LongSegmentError)?;
                }
                TypeValue::Builtin(BuiltinTypeValue::AsPath(as_path)) => {
                    let asn_vec = if let TypeValue::Builtin(
                        BuiltinTypeValue::Asn(asn),
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
            TypeValue::List(list) => {
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
                TypeValue::List(list) => {
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

impl VectorOption<AsPath> {
    pub fn as_routecore_hops_vec(
        &self,
    ) -> Vec<&routecore::bgp::aspath::Hop<Vec<u8>>> {
        if let Some(TypeValue::Builtin(BuiltinTypeValue::AsPath(hop_path))) =
            &self.value
        {
            hop_path.0.iter().collect::<Vec<_>>()
        } else {
            vec![]
        }
    }

    pub fn into_routecore_hops_vec(
        self,
    ) -> Vec<routecore::bgp::aspath::Hop<Vec<u8>>> {
        if let Some(TypeValue::Builtin(BuiltinTypeValue::AsPath(hop_path))) =
            self.value
        {
            hop_path.0.into_iter().collect::<Vec<_>>()
        } else {
            vec![]
        }
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

impl TryFrom<Vec<routecore::asn::Asn>> for AsPath {
    type Error = LongSegmentError;

    fn try_from(
        value: Vec<routecore::asn::Asn>,
    ) -> Result<Self, LongSegmentError> {
        routecore::bgp::aspath::HopPath::try_from(value.as_slice())
            .map(AsPath::from)
            .map_err(|_| LongSegmentError)
    }
}

impl From<Vec<routecore::bgp::aspath::Hop<Vec<u8>>>> for AsPath {
    fn from(value: Vec<routecore::bgp::aspath::Hop<Vec<u8>>>) -> Self {
        AsPath(routecore::bgp::aspath::HopPath::from(value))
    }
}

impl std::ops::Index<usize> for AsPath {
    type Output = routecore::bgp::aspath::Hop<std::vec::Vec<u8>>;

    fn index(&self, index: usize) -> &Self::Output {
        self.0.index(index)
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
