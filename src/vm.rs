use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    fmt::{Display, Formatter},
    hash::{Hash, Hasher},
    ops::{Index, IndexMut},
    sync::Arc,
};

use crate::{
    ast::{self, AcceptReject, CompareOp, ShortString},
    compiler::compile::{CompileError, MirBlock},
    first_into_vm_err,
    traits::{RotoType, Token},
    types::{
        builtin::{Boolean, BuiltinTypeValue},
        collections::{
            BytesRecord, ElementTypeValue, EnumBytesRecord, LazyRecord, List,
            Record,
        },
        datasources::{DataSource, DataSourceMethodValue},
        lazyrecord_types::{
            InitiationMessage, LazyRecordTypeDef, PeerDownNotification,
            PeerUpNotification, RouteMonitoring, StatisticsReport,
            TerminationMessage,
        },
        outputs::OutputStreamMessage,
        typedef::TypeDef,
        typevalue::TypeValue,
    },
};

use arc_swap::ArcSwapOption;
use log::{debug, error, log_enabled, trace, Level};
use serde::Serialize;
use smallvec::SmallVec;

//------------ FieldIndex ---------------------------------------------------

// This is a compound index that hop from sub-field to sub-field into a
// Collection, e.g. a Record { a: u8, b: u8, c: { d: u8, e: { x: u8, z: u8 }
// }, f: u8 }, could be queried with a field index [2,1,0], that would point
// the var field `x`.

#[derive(
    Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Serialize, Default,
)]
pub struct FieldIndex(SmallVec<[usize; 8]>);

impl FieldIndex {
    pub fn new() -> Self {
        Self(SmallVec::<[usize; 8]>::new())
    }

    pub fn first(&self) -> Result<usize, VmError> {
        self.0.first().ok_or(VmError::InvalidFieldAccess).copied()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn push(&mut self, index: usize) {
        self.0.push(index);
    }

    pub fn skip_first(&self) -> &[usize] {
        &self.0[1..]
    }

    pub fn last_mut(&mut self) -> Option<&mut usize> {
        self.0.last_mut()
    }
}

impl From<Vec<usize>> for FieldIndex {
    fn from(value: Vec<usize>) -> Self {
        Self(value.into())
    }
}

impl From<&[usize]> for FieldIndex {
    fn from(value: &[usize]) -> Self {
        Self(value.into())
    }
}

impl From<&Vec<u8>> for FieldIndex {
    fn from(value: &Vec<u8>) -> Self {
        Self(value.iter().map(|v| *v as usize).collect::<Vec<_>>().into())
    }
}

//------------ Stack --------------------------------------------------------

#[derive(Debug, Clone)]
pub enum StackRefPos {
    // index into LinearMemory
    MemPos(u32),
    // index into a Table (which is a vec of shared Records)
    TablePos(Token, usize),
    // CompareResult, which is not a Ref at all, but hey,
    // it's smaller than a ref, so who cares
    CompareResult(bool),
    // Index pointing to a constant, used for indexing a match variant
    ConstantIndex(u32),
    // An actual value
    ConstantValue(TypeValue),
}

impl From<u32> for StackRefPos {
    fn from(mem_pos: u32) -> Self {
        StackRefPos::MemPos(mem_pos)
    }
}

impl From<TypeValue> for StackRefPos {
    fn from(value: TypeValue) -> Self {
        StackRefPos::ConstantValue(value)
    }
}

// struct Stack(Vec<StackValue>);
// enum StackValue {
//     MemLoc(MemLoc),
// }

// MemLoc { mem_pos: u32, field_index: FieldIndex, variant_index: Option<u32> }

#[derive(Debug, Clone)]
pub(crate) struct StackRef {
    pub(crate) pos: StackRefPos,
    // nested field -> record.field sequences are expressed as a Vec of field
    // indexes, e.g. [1,2] on
    // Record { a: U8, b: Record { a: U8, b: U8, c: U8 }} would point to a.b
    field_index: FieldIndex,
}

#[derive(Debug)]
struct Stack(Vec<StackRef>);

impl<'a> Stack {
    fn new() -> Self {
        Stack(Vec::new())
    }

    fn push(&'a mut self, pos: StackRefPos) -> Result<(), VmError> {
        self.0.push(StackRef {
            pos,
            field_index: FieldIndex::new(),
        });
        Ok(())
    }

    fn pop(&'a mut self) -> Result<StackRef, VmError> {
        self.0.pop().ok_or(VmError::StackUnderflow)
    }

    fn get_top_value(&'a self) -> Result<&StackRef, VmError> {
        self.0.last().ok_or(VmError::StackUnderflow)
    }

    fn add_index_to_field_index(
        &mut self,
        index: usize,
    ) -> Result<(), VmError> {
        self.0
            .last_mut()
            .ok_or(VmError::StackUnderflow)?
            .field_index
            .push(index);
        Ok(())
    }

    fn push_with_field_index(
        &mut self,
        field_index: FieldIndex,
    ) -> Result<(), VmError> {
        self.0
            .last_mut()
            .ok_or(VmError::StackUnderflow)?
            .field_index = field_index;
        Ok(())
    }

    fn unwind(&mut self) -> Vec<StackRef> {
        std::mem::take(&mut self.0)
    }

    fn clear(&mut self) {
        self.0.clear();
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum StackValue<'a> {
    Ref(&'a TypeValue),
    Arc(Arc<TypeValue>),
    Owned(TypeValue),
}

impl<'a> AsRef<TypeValue> for StackValue<'a> {
    fn as_ref(&self) -> &TypeValue {
        match self {
            StackValue::Ref(r) => r,
            StackValue::Arc(r) => r,
            StackValue::Owned(r) => r,
        }
    }
}

// #[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
// pub struct StackValueRef<SV>(SV);

//------------ LinearMemory -------------------------------------------------

#[derive(Debug)]
pub struct LinearMemory([TypeValue; 512]);

impl LinearMemory {
    pub fn uninit() -> Self {
        const V: TypeValue = TypeValue::UnInit;
        LinearMemory([V; 512])
    }

    pub fn get_mem_pos(&self, index: usize) -> Option<&TypeValue> {
        self.0.get(index)
    }

    pub(crate) fn get_mp_field_as_bool(
        &self,
        stack_ref: &StackRef,
    ) -> Result<bool, VmError> {
        trace!(
            "mp field pos: {:?}, field_index: {:?}",
            stack_ref.pos,
            stack_ref.field_index
        );
        match stack_ref.pos {
            StackRefPos::MemPos(pos) => {
                if let StackValue::Owned(TypeValue::Builtin(
                    BuiltinTypeValue::Boolean(Boolean(b)),
                )) = self.get_mp_field_by_index_as_stack_value(
                    pos as usize,
                    stack_ref.field_index.clone(),
                )? {
                    Ok(b)
                } else {
                    Ok(false)
                }
            }
            StackRefPos::ConstantValue(TypeValue::Builtin(
                BuiltinTypeValue::Boolean(Boolean(b)),
            )) => Ok(b),
            StackRefPos::CompareResult(res) => Ok(res),
            _ => Ok(false),
        }
    }

    pub(crate) fn get_mp_field_as_unknown(
        &self,
        stack_ref: &StackRef,
    ) -> bool {
        trace!(
            "mp field pos: {:?}, field_index: {:?}",
            stack_ref.pos,
            stack_ref.field_index
        );
        if let StackRefPos::MemPos(pos) = stack_ref.pos {
            matches!(self.get_mem_pos(pos as usize), Some(TypeValue::Unknown))
        } else {
            false
        }
    }

    pub(crate) fn get_mp_field_by_stack_ref_owned(
        &mut self,
        stack_ref: &StackRef,
    ) -> Result<TypeValue, VmError> {
        let StackRef {
            pos: stack_ref_pos,
            field_index,
            ..
        } = stack_ref;

        match stack_ref_pos {
            StackRefPos::MemPos(pos) => match field_index {
                v if v.is_empty() => self
                    .get_mem_pos_as_owned(*pos as usize)
                    .ok_or(VmError::MemOutOfBounds),
                field_index => match self.get_mem_pos_as_owned(*pos as usize)
                {
                    Some(TypeValue::Record(mut r)) => {
                        let field =
                            r.get_field_by_index_owned(field_index.clone());
                        match field {
                            Some(ElementTypeValue::Nested(nested)) => {
                                Ok(*nested)
                            }
                            Some(ElementTypeValue::Primitive(b)) => Ok(b),
                            None => Err(VmError::InvalidFieldAccess),
                        }
                    }
                    Some(TypeValue::List(mut l)) => {
                        let field =
                            l.get_field_by_index_owned(field_index.clone());
                        match field {
                            Some(ElementTypeValue::Nested(nested)) => {
                                Ok(*nested)
                            }
                            Some(ElementTypeValue::Primitive(b)) => Ok(b),
                            _ => Err(VmError::MemOutOfBounds),
                        }
                    }
                    Some(TypeValue::Builtin(BuiltinTypeValue::Route(
                        route,
                    ))) => Ok(route
                        .get_field_by_index(field_index.first()?)
                        .unwrap_or(TypeValue::Unknown)),
                    _ => Err(VmError::MemOutOfBounds),
                },
            },
            StackRefPos::CompareResult(res) => Ok((*res).into()),
            _ => Err(VmError::MemOutOfBounds),
        }
    }

    pub fn get_mp_field_by_index_as_stack_value(
        &self,
        mem_pos: usize,
        field_index: FieldIndex,
    ) -> Result<StackValue, VmError> {
        trace!(
            "get_mp_field_by_index_as_stack_value {:?}",
            self.get_mem_pos(mem_pos).map(StackValue::Ref)
        );
        match field_index {
            fi if fi.is_empty() => {
                trace!("empty field index");
                if let Some(tv) = self.get_mem_pos(mem_pos) {
                    match tv {
                        // Do not own AsPath and Communities, cloning is
                        // expensive!
                        TypeValue::Builtin(BuiltinTypeValue::AsPath(_)) => {
                            Ok(StackValue::Ref(tv))
                        }
                        // Clone all other built-ins, they're cheap to clone
                        // (all copy) and the result is smaller than a pointer
                        TypeValue::Builtin(_) => {
                            trace!("builtin copy {}", tv);
                            Ok(StackValue::Owned(tv.clone()))
                        }
                        _ => Ok(StackValue::Ref(tv)),
                    }
                } else {
                    Err(VmError::InvalidMemoryAccess(mem_pos))
                }
            }
            field_index => {
                trace!(
                    "value in mem pos {}: {:?}",
                    mem_pos,
                    self.get_mem_pos(mem_pos)
                );
                match self.get_mem_pos(mem_pos) {
                    Some(TypeValue::Record(rec)) => {
                        trace!("record -> {}", rec);
                        match rec.get_field_by_index(&field_index) {
                            Some(ElementTypeValue::Nested(nested)) => {
                                trace!("=> nested field {}", nested);
                                Ok(StackValue::Ref(nested))
                            }
                            Some(ElementTypeValue::Primitive(b)) => {
                                trace!("=> primitive record field {:?}", b);
                                Ok(StackValue::Ref(b))
                            }
                            unknown_rec_field => {
                                error!(
                                    "Cannot find field with index: {:?}, in record {:?}",
                                    field_index,
                                    unknown_rec_field
                                );
                                Err(VmError::InvalidFieldAccess)
                            }
                        }
                    }
                    Some(TypeValue::List(l)) => {
                        let field = l.get_field_by_index(field_index);
                        match field {
                            Some(ElementTypeValue::Nested(nested)) => {
                                Ok(StackValue::Ref(nested))
                            }
                            Some(ElementTypeValue::Primitive(b)) => {
                                Ok(StackValue::Ref(b))
                            }
                            _ => Err(VmError::InvalidFieldAccess),
                        }
                    }
                    Some(TypeValue::Builtin(BuiltinTypeValue::Route(
                        route,
                    ))) => {
                        if let Ok(Some(v)) = route
                            .get_value_ref_for_field(field_index.first()?)
                        {
                            Ok(StackValue::Ref(v))
                        } else if let Ok(v) =
                            route.get_field_by_index(field_index.first()?)
                        {
                            Ok(StackValue::Owned(v))
                        } else {
                            Ok(StackValue::Owned(TypeValue::Unknown))
                        }
                    }
                    Some(TypeValue::Builtin(
                        BuiltinTypeValue::BgpUpdateMessage(bgp_msg),
                    )) => {
                        trace!(
                            "get bgp_update_message \
                        get_value_owned_for_field {:?} {:?}",
                            bgp_msg,
                            field_index
                        );
                        if let Some(v) = (*bgp_msg)
                            .get_value_owned_for_field(field_index.first()?)?
                        {
                            trace!("v {:?}", v);
                            Ok(StackValue::Owned(v))
                        } else {
                            Ok(StackValue::Owned(TypeValue::Unknown))
                        }
                    }
                    Some(TypeValue::Builtin(
                        BuiltinTypeValue::BmpRouteMonitoringMessage(bmp_msg),
                    )) => {
                        trace!(
                            "get bmp_route_monitoring_message \
                        get_value_owned_for_field {:?} {:?}",
                            bmp_msg,
                            field_index
                        );

                        let v = LazyRecord::from_type_def(BytesRecord::<
                            routecore::bmp::message::RouteMonitoring<
                                bytes::Bytes,
                            >,
                        >::lazy_type_def(
                        ))?
                        .get_field_by_index(&field_index, bmp_msg)?;

                        Ok(StackValue::Owned(v.try_into()?))
                    }
                    Some(TypeValue::Builtin(
                        BuiltinTypeValue::BmpPeerDownNotification(bmp_msg),
                    )) => {
                        trace!(
                            "get bmp_route_monitoring_message \
                        get_value_owned_for_field {:?} {:?}",
                            bmp_msg,
                            field_index
                        );

                        let v = LazyRecord::from_type_def(BytesRecord::<
                            routecore::bmp::message::PeerDownNotification<
                                bytes::Bytes,
                            >,
                        >::lazy_type_def(
                        ))?
                        .get_field_by_index(&field_index, bmp_msg)?;

                        Ok(StackValue::Owned(v.try_into()?))
                    }
                    Some(TypeValue::Builtin(
                        BuiltinTypeValue::BmpPeerUpNotification(bmp_msg),
                    )) => {
                        trace!(
                            "get bmp_peer_up_notification \
                        get_value_owned_for_field {:?} {:?}",
                            bmp_msg,
                            field_index
                        );

                        let v = LazyRecord::from_type_def(BytesRecord::<
                            routecore::bmp::message::PeerUpNotification<
                                bytes::Bytes,
                            >,
                        >::lazy_type_def(
                        ))?
                        .get_field_by_index(&field_index, bmp_msg)?;

                        Ok(StackValue::Owned(v.try_into()?))
                    }
                    Some(TypeValue::Builtin(
                        BuiltinTypeValue::BmpInitiationMessage(bmp_msg),
                    )) => {
                        trace!(
                            "get bmp_initiation_message \
                        get_value_owned_for_field {:?} {:?}",
                            bmp_msg,
                            field_index
                        );

                        let v = LazyRecord::from_type_def(BytesRecord::<
                            routecore::bmp::message::InitiationMessage<
                                bytes::Bytes,
                            >,
                        >::lazy_type_def(
                        ))?
                        .get_field_by_index(&field_index, bmp_msg)?;

                        Ok(StackValue::Owned(v.try_into()?))
                    }
                    Some(tv) => match tv {
                        // Do not own AsPath and Communities, cloning is
                        // expensive!
                        TypeValue::Builtin(BuiltinTypeValue::AsPath(_)) => {
                            Ok(StackValue::Ref(tv))
                        }
                        // Clone all other built-ins, they're cheap to clone
                        // (all copy) and the result is smaller than a pointer
                        TypeValue::Builtin(_) => {
                            trace!("builtin copy {}", tv);
                            Ok(StackValue::Owned(tv.clone()))
                        }
                        _ => Ok(StackValue::Ref(tv)),
                    },
                    // This is apparently a type that does not have fields
                    None => Err(VmError::InvalidFieldAccess),
                }
            }
        }
    }

    // Return a TypeValue if the memory holds the enum variant specified by
    // the varian_token. If the memory position holds an enum, but not of the
    // specified variant, then return TypeValue::Unknown.
    pub fn get_mp_as_variant_or_unknown(
        &self,
        mem_pos: usize,
        variant_token: Token,
    ) -> Result<StackValue, VmError> {
        trace!(
            "get_mp_as_variant_or_unknown {:?}",
            self.get_mem_pos(mem_pos).map(StackValue::Ref)
        );
        if let Some(tv) = self.get_mem_pos(mem_pos) {
            if let TypeValue::Builtin(BuiltinTypeValue::BmpMessage(
                bytes_rec,
            )) = tv
            {
                if bytes_rec.is_variant(variant_token) {
                    Ok(StackValue::Ref(tv))
                } else {
                    Ok(StackValue::Owned(TypeValue::Unknown))
                }
            } else {
                Err(VmError::InvalidValueType)
            }
        } else {
            Err(VmError::InvalidMemoryAccess(mem_pos))
        }
    }

    pub fn mp_is_variant(
        &self,
        mem_pos: usize,
        variant_token: Token,
    ) -> bool {
        trace!(
            "mp_is_variant {:?} == {:?}",
            self.get_mem_pos(mem_pos).map(StackValue::Ref),
            variant_token
        );
        match self.get_mem_pos(mem_pos) {
            // We only know how to deal with BmpMessages currently.
            Some(TypeValue::Builtin(BuiltinTypeValue::BmpMessage(
                bytes_rec,
            ))) => bytes_rec.is_variant(variant_token),
            Some(_) => false,
            _ => false,
        }
    }

    pub fn get_as_lazy_record_type(
        &self,
        mem_pos: usize,
    ) -> Result<LazyRecordTypeDef, VmError> {
        match self.get_mem_pos(mem_pos) {
            // We only know how to deal with BmpMessages currently.
            Some(TypeValue::Builtin(BuiltinTypeValue::BmpMessage(
                bytes_rec,
            ))) => Ok(bytes_rec.get_variant()),
            Some(_) => Err(VmError::InvalidRecord),
            _ => Err(VmError::InvalidMemoryAccess(mem_pos)),
        }
    }

    // Return the contents of the memory position as a TypeValue only if it is
    // a LazyRecord, or a field on a LazyRecord (a
    // LazyRecordElementTypeValue), otherwise return a VmError.
    pub fn get_lazy_field_by_index(
        &self,
        mem_pos: usize,
        field_index: FieldIndex,
    ) -> Result<StackValue, VmError> {
        trace!(
            "get_lazy_field_by_index {:?}",
            self.get_mem_pos(mem_pos).map(StackValue::Ref)
        );
        match field_index {
            // No field index, we want the whole bytes record
            fi if fi.is_empty() => {
                trace!("empty field index on bytes record");
                if let Some(tv) = self.get_mem_pos(mem_pos) {
                    match tv {
                        // A BmpMessage is an EnumBytesRecord, you can use the
                        // whole BytesRecord, e.g. to send it to an
                        // OutputStreamQueue, but you can't index into it. You
                        // can only index into its variants (see match arm
                        // `field_index` below).
                        TypeValue::Builtin(BuiltinTypeValue::BmpMessage(
                            _,
                        )) => Ok(StackValue::Ref(tv)),
                        TypeValue::Builtin(
                            BuiltinTypeValue::BmpRouteMonitoringMessage(_),
                        ) => Ok(StackValue::Ref(tv)),
                        TypeValue::Builtin(
                            BuiltinTypeValue::BmpPeerDownNotification(_),
                        ) => Ok(StackValue::Ref(tv)),
                        TypeValue::Builtin(
                            BuiltinTypeValue::BmpPeerUpNotification(_),
                        ) => Ok(StackValue::Ref(tv)),
                        TypeValue::Builtin(
                            BuiltinTypeValue::BmpInitiationMessage(_),
                        ) => Ok(StackValue::Ref(tv)),
                        TypeValue::Builtin(
                            BuiltinTypeValue::BmpTerminationMessage(_),
                        ) => Ok(StackValue::Ref(tv)),
                        TypeValue::Builtin(
                            BuiltinTypeValue::BmpStatisticsReport(_),
                        ) => Ok(StackValue::Ref(tv)),
                        _ => Err(VmError::InvalidValueType),
                    }
                } else {
                    Err(VmError::InvalidFieldAccess)
                }
            }
            field_index => {
                trace!(
                    "value in mem pos {}: {:?}",
                    mem_pos,
                    self.get_mem_pos(mem_pos)
                );

                match self.get_mem_pos(mem_pos) {
                    // You can't index into the field of a BmpMessage, it's an
                    // EnumBytesRecord: it doesn't have fields, only variants.
                    Some(TypeValue::Builtin(
                        BuiltinTypeValue::BmpMessage(_),
                    )) => Err(VmError::InvalidRecord),
                    Some(TypeValue::Builtin(
                        BuiltinTypeValue::BmpRouteMonitoringMessage(bmp_msg),
                    )) => {
                        trace!(
                            "get bmp_route_monitoring_message \
                        get_value_owned_for_field {:?} {:?}",
                            bmp_msg,
                            field_index
                        );

                        LazyRecord::from_type_def(BytesRecord::<
                            routecore::bmp::message::RouteMonitoring<
                                bytes::Bytes,
                            >,
                        >::lazy_type_def(
                        ))?
                        .get_field_by_index(&field_index, bmp_msg)
                        .map(|elm| elm.try_into())?
                        .map(StackValue::Owned)
                    }
                    Some(TypeValue::Builtin(
                        BuiltinTypeValue::BmpPeerDownNotification(bmp_msg),
                    )) => {
                        trace!(
                            "get bmp_peer_down_message \
                        get_value_owned_for_field {:?} {:?}",
                            bmp_msg,
                            field_index
                        );

                        LazyRecord::from_type_def(BytesRecord::<
                            routecore::bmp::message::PeerDownNotification<
                                bytes::Bytes,
                            >,
                        >::lazy_type_def(
                        ))?
                        .get_field_by_index(&field_index, bmp_msg)
                        .map(|elm| elm.try_into())?
                        .map(StackValue::Owned)
                    }
                    Some(TypeValue::Builtin(
                        BuiltinTypeValue::BmpPeerUpNotification(bmp_msg),
                    )) => {
                        trace!(
                            "get bmp_peer_up_message \
                        get_value_owned_for_field {:?} {:?}",
                            bmp_msg,
                            field_index
                        );

                        LazyRecord::from_type_def(BytesRecord::<
                            routecore::bmp::message::PeerUpNotification<
                                bytes::Bytes,
                            >,
                        >::lazy_type_def(
                        ))?
                        .get_field_by_index(&field_index, bmp_msg)
                        .map(|elm| elm.try_into())?
                        .map(StackValue::Owned)
                    }
                    Some(TypeValue::Builtin(
                        BuiltinTypeValue::BmpInitiationMessage(bmp_msg),
                    )) => {
                        trace!(
                            "get bmp_peer_up_message \
                        get_value_owned_for_field {:?} {:?}",
                            bmp_msg,
                            field_index
                        );

                        LazyRecord::from_type_def(BytesRecord::<
                            routecore::bmp::message::InitiationMessage<
                                bytes::Bytes,
                            >,
                        >::lazy_type_def(
                        ))?
                        .get_field_by_index(&field_index, bmp_msg)
                        .map(|elm| elm.try_into())?
                        .map(StackValue::Owned)
                    }
                    // This is apparently a type that does not have fields
                    _ => Err(VmError::InvalidFieldAccess),
                }
            }
        }
    }

    fn get_mem_pos_as_owned(&mut self, index: usize) -> Option<TypeValue> {
        // Memory positions 0 and 1 hold the rx and tx value respectively, you
        // can mutate those, but you can't move them out of here. If you
        // insist on doing this (the PushOutputStreamQueue command in the VM
        // wants this), then you get a clone
        match index {
            0 | 1 => self.0.get(index).cloned(),
            _ => self.0.get_mut(index).map(std::mem::take),
        }
    }

    fn set_mem_pos(&mut self, index: usize, value: TypeValue) {
        if let Some(mp) = self.0.get_mut(index) {
            *mp = value;
        }
    }

    // When returning the value on exiting the VM, the rx value should be
    // reset to uninitialized, otherwise a hard reference to the rx argument
    // that was passed into the VM, will linger on, panicking when the
    // LinearMemory is reused.
    pub fn take_rx_value(&mut self) -> Option<TypeValue> {
        self.0.get_mut(0).map(std::mem::take)
    }

    // Same for the tx value.
    pub fn take_tx_value(&mut self) -> Option<TypeValue> {
        self.0.get_mut(1).map(std::mem::take)
    }

    // When aborting the VM this should be invoked, so that this LinearMemory
    // instance can be reused.
    pub fn reset(&mut self) {
        self.0.get_mut(0).map(std::mem::take);
        self.0.get_mut(1).map(std::mem::take);
    }
}

impl Index<usize> for LinearMemory {
    type Output = TypeValue;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl IndexMut<usize> for LinearMemory {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
    }
}

impl Display for LinearMemory {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:?}", self.0)
    }
}

//------------ Command Arguments Stack --------------------------------------

// A stack especially for the arguments to a Command. The Vec that forms the
// stack is immutable, so that the VM doesn't need to clone the MIR code for
// each run. A counter keeps track of the current position in the stack.

#[derive(Debug)]
pub(crate) struct CommandArgsStack<'a> {
    args: &'a mut VecDeque<CommandArg>,
    args_counter: usize,
}

impl<'a> CommandArgsStack<'a> {
    // The counter counts down from the last element + 1.
    fn new(args: &'a mut VecDeque<CommandArg>) -> Self {
        let args_counter = args.len();
        Self { args, args_counter }
    }

    fn first(&self) -> Option<&CommandArg> {
        self.args.get(0)
    }

    // Remove and return the first item, decrement the counter This returns
    // None if the stack has under flowed.
    fn pop_front(&mut self) -> Option<CommandArg> {
        self.args_counter.checked_sub(1)?;
        self.args.pop_front()
    }

    // Return the last item and decrement the counter. This returns None if
    // the stack has under flowed.
    fn pop(&mut self) -> Result<&'_ CommandArg, VmError> {
        self.args_counter = self
            .args_counter
            .checked_sub(1)
            .ok_or(VmError::StackUnderflow)?;
        self.args
            .get(self.args_counter)
            .ok_or(VmError::InvalidValueType)
    }

    fn is_empty(&self) -> bool {
        self.args.is_empty()
    }

    // Interpret the last stack entry as a constant value.
    pub(crate) fn take_arg_as_constant(
        &mut self,
    ) -> Result<TypeValue, VmError> {
        // the first arg is the memory position index, the second arg is the
        // value to set on the memory position.
        match self.args.get(1) {
            Some(CommandArg::ConstantIndex(c)) => Ok(c.clone()),
            Some(CommandArg::List(l)) => Ok(TypeValue::List(l.clone())),
            Some(CommandArg::Record(r)) => Ok(TypeValue::Record(r.clone())),
            _ => Err(VmError::InvalidValueType),
        }
    }

    // Pop ALL arguments and return the top two. Returns None if the stack
    // underflows, or if one of the arguments does not exist.
    pub(crate) fn pop_2(
        mut self,
    ) -> Result<(&'a CommandArg, &'a CommandArg), VmError> {
        self.args_counter = self
            .args_counter
            .checked_sub(2)
            .ok_or(VmError::StackUnderflow)?;
        let r0;
        let r1;

        if let Some(a) = self.args.get(self.args_counter + 1) {
            r0 = a;
            if let Some(a) = self.args.get(self.args_counter) {
                r1 = a;
                return Ok((r0, r1));
            }
        }

        Err(VmError::StackUnderflow)
    }

    // Pop ALL arguments and return the top three arguments, Returns None if
    // the stack underflows, or if one of the arguments does not exist.
    pub(crate) fn pop_3(
        mut self,
    ) -> Result<(&'a CommandArg, &'a CommandArg, &'a CommandArg), VmError>
    {
        self.args_counter = self
            .args_counter
            .checked_sub(3)
            .ok_or(VmError::StackUnderflow)?;
        let r0;
        let r1;
        let r2;

        if let Some(a) = self.args.get(self.args_counter + 2) {
            r0 = a;
            if let Some(a) = self.args.get(self.args_counter + 1) {
                r1 = a;
                if let Some(a) = self.args.get(self.args_counter) {
                    r2 = a;
                    return Ok((r0, r1, r2));
                }
            }
        }

        Err(VmError::StackUnderflow)
    }

    pub(crate) fn get(&self, index: usize) -> Option<&CommandArg> {
        self.args.get(index)
    }
}

impl<'a> Index<usize> for CommandArgsStack<'a> {
    type Output = CommandArg;

    fn index(&self, index: usize) -> &Self::Output {
        self.args.index(index)
    }
}

impl<'a> From<&'a mut VecDeque<CommandArg>> for CommandArgsStack<'a> {
    fn from(value: &'a mut VecDeque<CommandArg>) -> Self {
        Self {
            args: value,
            args_counter: 0,
        }
    }
}

//------------ Argument -----------------------------------------------------

// These are the filter-map-level arguments, they can be compiled in when
// passed in before compiling (`with_arguments()`), or they can be provided
// at run-time.

#[derive(Debug, Clone)]
pub struct FilterMapArg {
    pub(crate) name: ShortString,
    index: Token,
    ty: TypeDef,
    value: TypeValue,
}

impl FilterMapArg {
    pub fn get_value(&self) -> &TypeValue {
        &self.value
    }

    pub fn take_value(&mut self) -> TypeValue {
        std::mem::take(&mut self.value)
    }

    pub fn get_name(&self) -> &str {
        self.name.as_str()
    }

    pub fn get_type(&self) -> TypeDef {
        self.ty.clone()
    }

    pub fn get_index(&self) -> Token {
        self.index.clone()
    }

    pub(crate) fn new(
        name: &str,
        index: Token,
        ty: TypeDef,
        value: TypeValue,
    ) -> Self {
        Self {
            name: name.into(),
            index,
            ty,
            value,
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct FilterMapArgs(Vec<FilterMapArg>);

impl FilterMapArgs {
    pub fn compile_arguments(
        &self,
        args: Vec<(&str, TypeValue)>,
    ) -> Result<FilterMapArgs, CompileError> {
        // Walk over all the filter_map arguments that were supplied and see
        // if they match up with the ones in the source code.
        let mut arguments_map = FilterMapArgs::new();
        let len = args.len();
        for supplied_arg in args {
            match self.iter().find(|a| supplied_arg.0 == a.get_name()) {
                // The argument is in the source code
                Some(found_arg) => {
                    // nice, but do the types match?
                    if found_arg.get_type() == supplied_arg.1 {
                        // yes, they match
                        arguments_map.insert(
                            found_arg.get_name(),
                            found_arg.get_index(),
                            found_arg.get_type(),
                            supplied_arg.1,
                        )
                    } else {
                        // Ok, but maybe we can convert into the type we
                        // need? Note that we can only try to convert if
                        // it's a builtin type.
                        match supplied_arg
                            .1
                            .into_builtin()
                            .and_then(|t| t.into_type(&found_arg.get_type()))
                        {
                            Ok(arg) => arguments_map.insert(
                                found_arg.get_name(),
                                found_arg.get_index(),
                                found_arg.get_type(),
                                arg,
                            ),
                            Err(_) => {
                                return Err(format!(
                                    "An invalid type was \
                                specified for argument: {}",
                                    supplied_arg.0
                                )
                                .into());
                            }
                        };
                    }
                }
                // The supplied argument is not in the source code.
                None => {
                    return Err(format!(
                        "Can't find argument in source: {}",
                        supplied_arg.0
                    )
                    .into())
                }
            }
        }

        // See if we got all the required arguments in the source code
        // covered.
        if arguments_map.len() != len {
            let missing_args = self
                .iter()
                .filter(|a| {
                    arguments_map.get_by_token_value(a.get_index()).is_none()
                })
                .map(|a| a.get_name())
                .collect::<Vec<&str>>();

            return Err(format!(
                "Some arguments are missing: {:?}",
                missing_args
            )
            .into());
        }

        // Sort the arguments on the Token::Argument(), so that in the future
        // the stack can use that index directly on the VMs arguments map.
        arguments_map.0.sort_by_key(|a| a.get_index());

        Ok(arguments_map)
    }

    pub fn inspect_arguments(&self) -> Vec<(&str, TypeDef)> {
        self.iter()
            .map(|a| (a.get_name(), a.get_type()))
            .collect::<Vec<_>>()
    }

    pub fn take_value_by_token(
        &mut self,
        index: Token,
    ) -> Result<TypeValue, VmError> {
        self.0
            .iter_mut()
            .find(|a| a.get_index() == index)
            .map(|a| a.take_value())
            .ok_or(VmError::AnonymousArgumentNotFound)
    }

    pub fn get_by_token_value(&self, index: Token) -> Option<&TypeValue> {
        self.0
            .iter()
            .find(|a| a.get_index() == index)
            .map(|a| a.get_value())
    }

    fn _take_by_index(mut self, index: usize) -> Option<TypeValue> {
        self.0.get_mut(index).map(|a| a.take_value())
    }

    pub fn new() -> Self {
        FilterMapArgs(Vec::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn insert(
        &mut self,
        name: &str,
        index: Token,
        ty: TypeDef,
        value: TypeValue,
    ) {
        self.0.push(FilterMapArg {
            name: name.into(),
            index,
            ty,
            value,
        });
    }

    pub fn last_index(&self) -> Option<usize> {
        if self.0.is_empty() {
            Some(self.0.len())
        } else {
            None
        }
    }

    pub fn iter(&self) -> std::slice::Iter<'_, FilterMapArg> {
        self.0.iter()
    }
}

impl IntoIterator for FilterMapArgs {
    type IntoIter = std::vec::IntoIter<Self::Item>;
    type Item = FilterMapArg;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl From<Vec<FilterMapArg>> for FilterMapArgs {
    fn from(value: Vec<FilterMapArg>) -> Self {
        Self(value)
    }
}

impl std::hash::Hash for FilterMapArgs {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for a in &self.0 {
            a.value.hash(state);
        }
    }
}

// Computes the has over the mir code and the data sources, that is stored in
// the built VM. This hash serves the purpose to figure out if a new (mir
// code,data sources) tuple actually contains meaningful changes.
pub fn compute_hash(
    mir_code: &[MirBlock],
    data_sources: &[ExtDataSource],
) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    mir_code.hash(&mut hasher);
    data_sources.hash(&mut hasher);
    hasher.finish()
}

//------------ CompiledPrimitiveField ----------------------------------------

// Variable Assignments can be of any these types:
// 1. literal primitive values: a = “AA”;
// 2. literal compound values: a = [1,2,3]; a = Msg { prefix: fe80::/24,
//    peer_ip: fe80::1, comment: “bla” };
// 3. primitive aliasing a = route.prefix;
// 4. compound aliasing a = Msg { prefix: route.prefix, peer_ip:
//    route.peer_ip, comment: “bla”  };

// In the first case we're creating a new value, that we'll have to store in a
// memory position and create a single entry in the VariablesRefTable. In the
// second case we can do the same thing, since a memory position can hold a
// complete literal record, provided all of its fields are literal values (so
// no computation needed at run time). The third case only needs a single
// entry in the VariablesRefTable, pointing to a pre-filled memory position.
// The last case is the hardest, we need to be able to store a series of
// references to memory positions that can addressed from a single entry in
// the VariablesRefTable.

// In short:
// 1. single mem_pos_set + entry in var_ref_table
// 2. single mem_pos_set + entry in var_ref_table
// 3. entry in var_ref_table
// 4. mem_pos_sets + entry in var_ref_table

// This the table that maps the user-defined variables from the 'define'
// section to memory positions in the VM.
#[derive(Debug, Clone)]
pub(crate) struct CompiledPrimitiveField {
    field_index: FieldIndex,
    commands: Vec<Command>,
}

impl CompiledPrimitiveField {
    pub fn new(commands: Vec<Command>, field_index: FieldIndex) -> Self {
        Self {
            commands,
            field_index,
        }
    }

    pub fn push_command(&mut self, op: OpCode, args: Vec<CommandArg>) {
        self.commands.push(Command::new(op, args));
    }

    pub fn extend_commands(&mut self, commands: Vec<Command>) {
        self.commands.extend(commands);
    }

    pub fn get_commands(&self) -> &Vec<Command> {
        &self.commands
    }

    pub fn get_field_index(&self) -> &FieldIndex {
        &self.field_index
    }
}

#[derive(Debug, Clone)]
pub(crate) struct CompiledCollectionField {
    field_index: FieldIndex,
}

impl CompiledCollectionField {
    pub(crate) fn new(field_index: FieldIndex) -> Self {
        Self { field_index }
    }

    pub(crate) fn get_field_index(&self) -> &FieldIndex {
        &self.field_index
    }
}

#[derive(Debug, Clone)]
pub(crate) enum CompiledField {
    Primitive(CompiledPrimitiveField),
    Collection(CompiledCollectionField),
}

impl CompiledField {
    pub(crate) fn get_field_index(&self) -> &FieldIndex {
        match self {
            CompiledField::Primitive(p) => p.get_field_index(),
            CompiledField::Collection(c) => c.get_field_index(),
        }
    }
}

//------------ CompiledVariable ----------------------------------------------

// This is the data-structure that stores snippets of compiled code (sequences
// of VM commands) that compute a variable, in a flat, stack-friendly way. For
// scalar values the length of the of the Vec<CompiledField> will be one, for
// records it will correspond to the number of fields in that record.
#[derive(Debug, Clone)]
pub(crate) struct CompiledVariable(Vec<CompiledField>);

impl CompiledVariable {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn get_accumulated_commands(&self) -> Vec<Command> {
        let mut acc_commands = vec![];

        for vr in self.0.iter() {
            match vr {
                CompiledField::Primitive(p) => {
                    acc_commands.extend(p.get_commands().clone())
                }
                CompiledField::Collection(_c) => {}
            }
        }

        acc_commands
    }

    pub fn get_commands_for_field_index(
        &self,
        field_index: &[usize],
    ) -> Result<Vec<Command>, VmError> {
        trace!("get_commands_for_field_index {:?} {:#?}", field_index, self);

        let field_index = FieldIndex::from(field_index);
        // we need to know first what type this field has.
        let field = self
            .0
            .iter()
            .find(|vr| vr.get_field_index() == &field_index);

        let cmds = match field {
            Some(CompiledField::Primitive(p)) => p.get_commands().clone(),
            Some(CompiledField::Collection(_c)) => {
                let mut acc_commands = vec![];

                let start_vr = self
                    .0
                    .iter()
                    .position(|vr| vr.get_field_index() == &field_index)
                    .ok_or_else(|| VmError::InvalidCommand)?;

                for vr in self.0.split_at(start_vr).1 {
                    if let CompiledField::Primitive(p) = vr {
                        acc_commands.extend(p.get_commands().clone());
                    }
                }

                acc_commands
            }
            None => vec![],
        };

        Ok(cmds)
    }

    pub fn append_primitive(&mut self, primitive: CompiledPrimitiveField) {
        self.0.push(CompiledField::Primitive(primitive));
    }

    pub fn append_collection(&mut self, collection: CompiledCollectionField) {
        self.0.push(CompiledField::Collection(collection));
    }

    // pub fn iter(&self) -> impl Iterator<Item = &CompiledField> + '_ {
    //     self.0.iter()
    // }

    pub fn iter_mut(&mut self) -> std::slice::IterMut<CompiledField> {
        self.0.iter_mut()
    }

    pub fn sort(&mut self) {
        self.0
            .sort_by(|a, b| a.get_field_index().cmp(b.get_field_index()));
    }
}

impl From<CompiledPrimitiveField> for CompiledVariable {
    fn from(value: CompiledPrimitiveField) -> Self {
        CompiledVariable(vec![CompiledField::Primitive(value)])
    }
}

impl From<CompiledCollectionField> for CompiledVariable {
    fn from(value: CompiledCollectionField) -> Self {
        CompiledVariable(vec![CompiledField::Collection(value)])
    }
}

#[derive(Debug, Default)]
pub(crate) struct VariablesRefTable(HashMap<Token, CompiledVariable>);

impl VariablesRefTable {
    pub(crate) fn get_by_token_value(
        &self,
        index: usize,
    ) -> Option<CompiledVariable> {
        self.0.get(&Token::Variable(index)).cloned()
    }

    pub(crate) fn set_primitive(
        &mut self,
        var_token_value: usize,
        commands: Vec<Command>,
        field_index: FieldIndex,
    ) -> Result<(), CompileError> {
        self.0.insert(
            Token::Variable(var_token_value),
            CompiledPrimitiveField {
                commands,
                field_index,
            }
            .into(),
        );
        Ok(())
    }

    pub(crate) fn insert(
        &mut self,
        token: Token,
        mut var: CompiledVariable,
    ) -> Result<(), CompileError> {
        var.sort();
        self.0.insert(token, var);
        Ok(())
    }
}

//------------ Virtual Machine ----------------------------------------------

pub struct VirtualMachine<MB: AsRef<[MirBlock]>, EDS: AsRef<[ExtDataSource]>>
{
    // _rx_type: TypeDef,
    // _tx_type: Option<TypeDef>,
    mir_code: MB,
    data_sources: EDS,
    arguments: FilterMapArgs,
    stack: RefCell<Stack>,
    hash_id: u64,
}

impl<
        'a,
        MB: AsRef<[MirBlock]> + std::hash::Hash,
        EDS: AsRef<[ExtDataSource]> + std::hash::Hash,
    > VirtualMachine<MB, EDS>
{
    fn _move_rx_tx_to_mem(
        &'a mut self,
        rx: impl RotoType,
        tx: Option<impl RotoType>,
        mem: &mut LinearMemory,
    ) {
        let rx = rx.take_value();
        mem.set_mem_pos(0, rx.into());

        if let Some(tx) = tx {
            let tx = tx.take_value();
            mem.set_mem_pos(1, tx.into());
        }
    }

    fn _unwind_resolved_stack_into_vec(
        &'a self,
        mem: &'a LinearMemory,
    ) -> Result<Vec<StackValue>, VmError> {
        let stack = self.stack.borrow_mut().unwind();
        let mut unwind_stack: Vec<StackValue> =
            Vec::with_capacity(stack.len());
        for sr in stack.into_iter() {
            match sr.pos {
                StackRefPos::MemPos(pos) => {
                    let v = mem.get_mp_field_by_index_as_stack_value(
                        pos as usize,
                        sr.field_index,
                    )?;
                    unwind_stack.push(v);
                }
                StackRefPos::TablePos(token, pos) => {
                    let ds =
                        get_data_source(self.data_sources.as_ref(), token)?;
                    match ds
                        .get_at_field_index(pos, sr.field_index)
                        .map_err(|_| VmError::InvalidDataSourceAccess)?
                    {
                        Some(v) => {
                            unwind_stack.push(StackValue::Arc(v.into()))
                        }
                        None => unwind_stack
                            .push(StackValue::Owned(TypeValue::Unknown)),
                    }
                }
                StackRefPos::CompareResult(res) => {
                    unwind_stack.push(StackValue::Owned(res.into()))
                }
                StackRefPos::ConstantIndex(c) => {
                    unwind_stack.push(StackValue::Owned(c.into()))
                }
                StackRefPos::ConstantValue(v) => {
                    unwind_stack.push(StackValue::Owned(v))
                }
            }
        }
        Ok(unwind_stack)
    }

    // Take a number of elements on the stack and flush the rest, so we'll end
    // up with an empty stack.
    fn _take_resolved_and_flush(
        &'a self,
        elem_num: u32, // number of elements to take
        mem: &'a LinearMemory,
    ) -> Result<Vec<StackValue>, VmError> {
        let mut stack = self.stack.borrow_mut();

        let len = stack.0.len();
        let stack_part = stack.0.split_off(len - elem_num as usize);
        let mut take_vec = vec![];

        for sr in stack_part {
            match sr.pos.clone() {
                StackRefPos::MemPos(pos) => {
                    take_vec.push(mem.get_mp_field_by_index_as_stack_value(
                        pos as usize,
                        sr.field_index.clone(),
                    )?)
                }
                StackRefPos::TablePos(token, pos) => {
                    let ds =
                        get_data_source(self.data_sources.as_ref(), token)?;
                    let v = ds
                        .get_at_field_index(pos, sr.field_index.clone())
                        .map_err(|_| VmError::InvalidDataSourceAccess)?;
                    if let Some(v) = v {
                        take_vec.push(StackValue::Arc(v.into()))
                    } else {
                        take_vec.push(StackValue::Owned(TypeValue::Unknown))
                    }
                }
                StackRefPos::CompareResult(res) => {
                    take_vec.push(StackValue::Owned(res.into()))
                }
                StackRefPos::ConstantIndex(c) => {
                    take_vec.push(StackValue::Owned(c.into()))
                }
                StackRefPos::ConstantValue(v) => {
                    take_vec.push(StackValue::Owned(v))
                }
            };
        }

        stack.clear();
        Ok(take_vec)
    }

    // Take a number of elements on the stack, leaving the rest of the stack
    // in place.
    fn _take_resolved(
        &'a self,
        elem_num: u32, // number of elements to take
        mem: &'a LinearMemory,
    ) -> Result<Vec<StackValue>, VmError> {
        let mut stack = self.stack.borrow_mut();

        let len = stack.0.len();
        let stack_part = stack.0.split_off(
            len.checked_sub(elem_num as usize).ok_or_else(|| {
                debug!(
                    "Stack underflow. Requested {} arguments, but {} were \
                    present on the stack.",
                    elem_num, len
                );
                VmError::StackUnderflow
            })?,
        );
        let mut take_vec = vec![];

        for sr in stack_part {
            match sr.pos.clone() {
                StackRefPos::MemPos(pos) => {
                    take_vec.push(mem.get_mp_field_by_index_as_stack_value(
                        pos as usize,
                        sr.field_index.clone(),
                    )?);
                }
                StackRefPos::TablePos(token, pos) => {
                    let ds =
                        get_data_source(self.data_sources.as_ref(), token)?;
                    let v = ds
                        .get_at_field_index(pos, sr.field_index.clone())
                        .map_err(|_| VmError::InvalidDataSourceAccess)?;
                    if let Some(v) = v {
                        take_vec.push(StackValue::Arc(v.into()));
                    } else {
                        take_vec.push(StackValue::Owned(TypeValue::Unknown));
                    }
                }
                StackRefPos::CompareResult(res) => {
                    take_vec.push(StackValue::Owned(res.into()));
                }
                StackRefPos::ConstantIndex(c) => {
                    take_vec.push(StackValue::Owned(c.into()));
                }
                StackRefPos::ConstantValue(v) => {
                    take_vec.push(StackValue::Owned(v))
                }
            };
        }

        Ok(take_vec)
    }

    // Take a number of elements on the stack, leaving the rest of the stack
    // in place.
    fn _take_resolved_as_owned(
        &'a self,
        elem_num: u32, // number of elements to take
        mem: &'a mut LinearMemory,
    ) -> Result<Vec<TypeValue>, VmError> {
        let mut stack = self.stack.borrow_mut();

        let len = stack.0.len();
        let stack_part = stack.0.split_off(len - elem_num as usize);
        let mut take_vec = vec![];

        for sr in stack_part {
            match sr.pos.clone() {
                StackRefPos::MemPos(_pos) => {
                    take_vec.push(mem.get_mp_field_by_stack_ref_owned(&sr)?);
                }
                StackRefPos::TablePos(_token, _pos) => {
                    return Err(VmError::InvalidWrite);
                }
                StackRefPos::CompareResult(res) => {
                    take_vec.push(res.into());
                }
                StackRefPos::ConstantIndex(c) => {
                    take_vec.push(c.into());
                }
                StackRefPos::ConstantValue(v) => {
                    take_vec.push(v);
                }
            }
        }

        Ok(take_vec)
    }

    fn as_vec(&'a self) -> Vec<StackRef> {
        let mut stack = self.stack.borrow_mut();
        stack.unwind()
    }

    pub fn reset_stack(&self) {
        self.stack.borrow_mut().clear();
    }

    fn get_data_source(
        &self,
        token: usize,
    ) -> Result<Arc<DataSource>, VmError> {
        self.data_sources
            .as_ref()
            .iter()
            .find(|ds| ds.token == token)
            .and_then(|ds| ds.source.load_full().as_ref().map(Arc::clone))
            .ok_or(VmError::DataSourceTokenNotFound(token))
    }

    pub fn get_hash_id(&self) -> u64 {
        self.hash_id
    }

    // re-populate the VM with different intermediate code and different data
    // sources. Note that the MIR and the data sources MUST come from the same
    // RotoPack. If they are not, then the VM will crash or return arbitrary
    // results, a.k.a. Undefined Behaviour! THIS IS NOT CHECKED BY THIS
    // METHOD, ITS ON THE CONSUMER OF THIS METHOD TO GUARANTEE THIS.

    // Returns a bool that indicates whether the MIR and the data sources
    // where actually replaced.
    pub fn replace_mir_code_and_data_sources(
        &'a mut self,
        mir_code: MB,
        data_sources: EDS,
    ) -> bool {
        let new_hash = compute_hash(mir_code.as_ref(), data_sources.as_ref());

        // If the hash is the same the MIR code has *not* changed, return
        // without replacing anything.
        if new_hash == self.hash_id {
            return false;
        }

        // This is different from the *mir_cde, data_sources) tuple, so store
        // that.
        self.data_sources = data_sources;
        self.mir_code = mir_code;
        self.hash_id = new_hash;

        true
    }

    pub fn exec(
        &'a mut self,
        rx: impl RotoType,
        tx: Option<impl RotoType>,
        // define filter-map-level arguments, not used yet! Todo
        mut _arguments: Option<FilterMapArgs>,
        mem: &mut LinearMemory,
    ) -> Result<VmResult, VmError> {
        trace!("\nstart executing vm...");

        let mut commands_num: usize = 0;

        self._move_rx_tx_to_mem(rx, tx, mem);
        let mut output_stream_queue: OutputStreamQueue =
            OutputStreamQueue::new();

        for mir_block in self.mir_code.as_ref() {
            trace!("\n\n--mirblock------------------");
            trace!("stack: {:?}", self.stack);
            let mut skip_label = false;

            for (pc, Command { op, args }) in mir_block.iter().enumerate() {
                if skip_label {
                    if let OpCode::Label = op {
                        trace!("stop skip");
                        skip_label = false;
                    } else {
                        continue;
                    }
                }

                commands_num += 1;
                let args = &mut args.clone();
                let mut args = CommandArgsStack::new(args);
                trace!("\n{:3} -> {:?} {:?} ", pc, op, args);
                match op {
                    // args: [CompareOperator]
                    // stack args: [cmp1, cmp2]
                    OpCode::Cmp => {
                        let stack_args = self._take_resolved(2, mem)?;

                        trace!("raw stack args {:#?}", stack_args);
                        let left: &TypeValue = stack_args
                            .get(0)
                            .map_or(Err(VmError::InvalidCommand), |a| {
                                Ok(a.as_ref())
                            })?;
                        let right = stack_args
                            .get(1)
                            .map_or(Err(VmError::InvalidCommand), |a| {
                                Ok(a.as_ref())
                            })?;

                        if log_enabled!(Level::Trace) {
                            trace!(" {:?} <-> {:?}", left, right);
                        }

                        match first_into_vm_err!(args, InvalidCommandArg)? {
                            CommandArg::CompareOp(CompareOp::Eq) => {
                                let res = left == right;
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::CompareResult(res))?;
                            }
                            CommandArg::CompareOp(CompareOp::Ne) => {
                                let res = left != right;
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::CompareResult(res))?;
                            }
                            CommandArg::CompareOp(CompareOp::Lt) => {
                                let res = left < right;
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::CompareResult(res))?;
                            }
                            CommandArg::CompareOp(CompareOp::Le) => {
                                let res = left <= right;
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::CompareResult(res))?;
                            }
                            CommandArg::CompareOp(CompareOp::Gt) => {
                                let res = left > right;
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::CompareResult(res))?;
                            }
                            CommandArg::CompareOp(CompareOp::Ge) => {
                                let res = left >= right;
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::CompareResult(res))?;
                            }
                            CommandArg::CompareOp(CompareOp::Or) => {
                                let l = left.try_into()?;
                                let r = right.try_into()?;
                                let res = l || r;
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::CompareResult(res))?;
                            }
                            CommandArg::CompareOp(CompareOp::And) => {
                                let res =
                                    left.try_into()? && right.try_into()?;
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::CompareResult(res))?;
                            }
                            // Two possibilities here:
                            // - the right hand side is an actual
                            //   TypeValue::List, we can compare the left
                            //   hand to all the values in
                            //   the right hand side list.
                            // - the right hand side list consists of all the
                            //   elements in stack_args, but for the first
                            //   one (that's still the left hand side)
                            CommandArg::CompareOp(CompareOp::In) => {
                                let res = if let TypeValue::List(list) = right
                                {
                                    list.iter().any(|v| {
                                        assert_ne!(v, &TypeValue::UnInit);
                                        v == left
                                    })
                                } else {
                                    stack_args[1..].iter().any(|v| {
                                        assert_ne!(
                                            v.as_ref(),
                                            &TypeValue::UnInit
                                        );
                                        v.as_ref() == left
                                    })
                                };
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::CompareResult(res))?;
                            }
                            CommandArg::CompareOp(CompareOp::NotIn) => {
                                let res = if let TypeValue::List(list) = right
                                {
                                    !list.iter().any(|v| {
                                        assert_ne!(v, &TypeValue::UnInit);
                                        v == left
                                    })
                                } else {
                                    !stack_args[1..].iter().any(|v| {
                                        assert_ne!(
                                            v.as_ref(),
                                            &TypeValue::UnInit
                                        );
                                        v.as_ref() == left
                                    })
                                };
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::CompareResult(res))?;
                            }
                            _ => {
                                return Err(VmError::InvalidCompareOp(pc));
                            }
                        }
                    }
                    // stack args: [type, method_token, args]
                    OpCode::ExecuteTypeMethod => {
                        if log_enabled!(Level::Trace) {
                            trace!("Stack {:?}", self.stack);
                            trace!("Args {:?}", args);
                        }

                        let (command_args, method_t, return_type) =
                            args.pop_3()?;

                        let stack_args = self._take_resolved(
                            command_args.get_args_len() as u32,
                            mem,
                        )?;

                        // We are going to call a method on a type, so we
                        // extract the type from the first argument on the
                        // stack.
                        if let CommandArg::Type(t) = return_type {
                            let val = t.exec_type_method(
                                method_t.try_into()?,
                                &stack_args,
                                return_type.try_into()?,
                            )?;
                            // mem.set_mem_pos(mem_pos, val);
                            self.stack.borrow_mut().push(val.into())?;
                        }
                    }
                    // stack args: [method_token, return_type, arguments]
                    OpCode::ExecuteValueMethod => {
                        trace!("execute value method {:?}", args);

                        let args_len: usize = if let CommandArg::Arguments(
                            args,
                        ) = args.pop()?
                        {
                            args.len()
                        } else {
                            0
                        };

                        let (return_type, method_token) = args.pop_2()?;
                        trace!(
                            "return_type {:?}, method_token {:?}",
                            return_type,
                            method_token,
                        );

                        let mut stack = self.stack.borrow_mut();

                        // TODO THIS IS MOST PROBABLY WRONG!!
                        // There always needs to be one argument on the stack.
                        let mut stack_args = vec![];
                        for _arg_num in 0..=args_len {
                            let sr = stack.pop()?;

                            let stack_arg = match sr.pos {
                                StackRefPos::MemPos(pos) => mem
                                    .get_mp_field_by_index_as_stack_value(
                                        pos as usize,
                                        sr.field_index,
                                    )
                                    .map_err(|_| {
                                        trace!("\nstack: {:?}", stack);
                                        trace!("mem: {:#?}", mem.0);
                                        VmError::InvalidMemoryAccess(
                                            pos as usize,
                                        )
                                    })?,
                                StackRefPos::TablePos(token, pos) => {
                                    let ds = get_data_source(
                                        self.data_sources.as_ref(),
                                        token,
                                    )?;
                                    let v = ds
                                        .get_at_field_index(
                                            pos,
                                            sr.field_index,
                                        )
                                        .map_err(|_| {
                                            VmError::InvalidDataSourceAccess
                                        })?;
                                    if let Some(v) = v {
                                        StackValue::Arc(v.into())
                                    } else {
                                        StackValue::Owned(TypeValue::Unknown)
                                    }
                                }
                                StackRefPos::CompareResult(res) => {
                                    StackValue::Owned(res.into())
                                }
                                StackRefPos::ConstantIndex(c) => {
                                    StackValue::Owned(c.into())
                                }
                                StackRefPos::ConstantValue(v) => {
                                    StackValue::Owned(v)
                                }
                            };

                            stack_args.push(stack_arg);
                        }
                        trace!("stack_args {:?}", stack_args);

                        // The last value on the stack is the value which we
                        // are going to call a method with.
                        let call_value = stack_args
                            .last()
                            .ok_or(VmError::InvalidMethodCall)?
                            .as_ref();

                        trace!(
                            "typevalue to call method with token {:?} on \
                        {:?} with args {:?}",
                            method_token,
                            call_value,
                            &stack_args[..stack_args.len() - 1]
                        );
                        let v = call_value.exec_value_method(
                            method_token.try_into()?,
                            // all the args on the command stack minus the
                            // last one
                            &stack_args[..stack_args.len() - 1],
                            return_type.try_into()?,
                        )?;
                        trace!("result {v}");

                        // mem.set_mem_pos(mem_pos, v);
                        stack.push(StackRefPos::ConstantValue(v))?;
                    }
                    // stack args: [ method_token, return_type, arguments]
                    //      pops arguments from the stack
                    OpCode::ExecuteConsumeValueMethod => {
                        let args_len: usize = if let CommandArg::Arguments(
                            args,
                        ) = args.pop()?
                        {
                            args.len()
                        } else {
                            0
                        };

                        if log_enabled!(Level::Trace) {
                            trace!("Args {:?}", args);
                        }
                        let (return_type, method_token) = args.pop_2()?;

                        // pop as many references from the stack as we have
                        // arguments for this method and resolve them to their
                        // values.
                        let mut stack = self.stack.borrow_mut();

                        // the field_index of the first argument on the
                        // stack, this is the field that will be consumed
                        // and returned by `exec_consume_value_method`
                        // later on.
                        let mut target_field_index = FieldIndex::new();

                        trace!("\nargs_len {}", args_len);
                        trace!("Stack {:?}", stack);

                        let mut stack_args = vec![];

                        for _arg_num in 0..args_len {
                            let sr = stack.pop()?;

                            target_field_index =
                                if target_field_index.is_empty() {
                                    sr.field_index
                                } else {
                                    target_field_index.clone()
                                };

                            let stack_arg = {
                                match sr.pos {
                                    StackRefPos::MemPos(pos) => mem
                                        .get_mem_pos_as_owned(pos as usize)
                                        .ok_or_else(|| {
                                            trace!("\nstack: {:?}", stack);
                                            // trace!("mem: {:#?}", mem.0);
                                            VmError::InvalidMemoryAccess(
                                                pos as usize,
                                            )
                                        })?,
                                    StackRefPos::TablePos(_token, _pos) => {
                                        // You can't write to a data source,
                                        // so you can't consume values from
                                        // it, that would be removing the
                                        // value from the data source.
                                        return Err(
                                            VmError::InvalidDataSourceAccess,
                                        );
                                    }
                                    StackRefPos::CompareResult(_res) => {
                                        return Err(VmError::InvalidCommand)
                                    }
                                    StackRefPos::ConstantIndex(_c) => {
                                        return Err(
                                            VmError::InvalidValueType,
                                        );
                                    }
                                    StackRefPos::ConstantValue(v) => v,
                                }
                            };

                            stack_args.push(stack_arg);
                        }

                        // The first value on the stack is the value which we
                        // are going to call a method with.
                        let collection_value = stack_args.remove(0);

                        // We have the complete instance, now see if we can:
                        // - get hold of the right field in this instance
                        //   (it is taken out of the instance!)
                        // - invoke the method that was requested on the
                        //   field
                        // - put the result of the method back in the
                        //   instance
                        // - write the instance back in the right memory
                        //   position.
                        let result_value = match collection_value {
                            TypeValue::Record(mut rec) => {
                                let call_value = TypeValue::try_from(
                                    rec.get_field_by_index_owned(
                                        target_field_index.clone(),
                                    )
                                    .ok_or_else(|| VmError::InvalidRecord)?,
                                )?
                                .exec_consume_value_method(
                                    method_token.try_into()?,
                                    stack_args,
                                    return_type.try_into()?,
                                    // target_field_index.clone(),
                                )?;
                                rec.set_value_on_field_index(
                                    target_field_index.clone(),
                                    call_value,
                                )?;
                                TypeValue::Record(rec)
                            }
                            TypeValue::List(mut list) => {
                                let call_value = TypeValue::try_from(
                                    list.get_field_by_index_owned(
                                        target_field_index.clone(),
                                    )
                                    .ok_or(VmError::InvalidFieldAccess)?,
                                )?
                                .exec_consume_value_method(
                                    method_token.try_into()?,
                                    stack_args,
                                    return_type.try_into()?,
                                    // target_field_index.clone(),
                                )?;
                                list.set_field_for_index(
                                    target_field_index,
                                    call_value,
                                )?;
                                TypeValue::List(list)
                            }
                            _ => collection_value,
                        };

                        // mem.set_mem_pos(mem_pos, result_value);
                        stack
                            .push(StackRefPos::ConstantValue(result_value))?;
                    }
                    // command args: [field_index, lazy_record_type,
                    // return_type]
                    OpCode::LoadLazyFieldValue => {
                        trace!("load lazy field value {:?}", args);

                        let return_type = args.pop();
                        trace!("return_type {:?}", return_type,);

                        let lazy_record_type = if let CommandArg::Type(
                            TypeDef::LazyRecord(lazy_record_type),
                        ) = args.pop()?
                        {
                            trace!("lazy record type {:?}", lazy_record_type);
                            *lazy_record_type
                        } else {
                            return Err(VmError::InvalidMethodCall);
                        };

                        // This is the field index on the lazy record
                        let field_index =
                            if let CommandArg::FieldIndex(fi) = args.pop()? {
                                fi
                            } else {
                                return Err(VmError::InvalidMethodCall);
                            };

                        let mut stack = self.stack.borrow_mut();
                        let StackRef {
                            field_index: v_field_index,
                            pos,
                            ..
                        } = stack.pop()?;

                        let bytes_rec_sv = {
                            match pos {
                                // Rx, Tx, and Arguments containing a
                                // LazyRecord end up here.
                                StackRefPos::MemPos(mp) => {
                                    mem.get_lazy_field_by_index(
                                        mp as usize,
                                        // The field index from the top of the
                                        // stack represents the variant if
                                        // this is a EnumBytesRecord
                                        v_field_index,
                                    )
                                    .map_err(|e| {
                                        trace!("\nstack: {:?}", stack);
                                        trace!("mem: {:#?}", mem.0);
                                        e
                                    })?
                                }
                                // Currently this arm is not used.
                                // SharedValues may go here in the future. If
                                // not delete this.
                                StackRefPos::ConstantValue(type_value) => {
                                    StackValue::Owned(type_value)
                                }
                                _ => {
                                    trace!(
                                        "WHAT? {:?} {:?}",
                                        v_field_index,
                                        pos
                                    );
                                    return Err(VmError::InvalidVariant);
                                }
                            }
                        };

                        // Only push a value to the stack if this the memory
                        // position holds a LazyRecord.
                        if let TypeValue::Builtin(b_tv) =
                            bytes_rec_sv.as_ref()
                        {
                            match b_tv {
                                // Currently this arm is not used!
                                BuiltinTypeValue::BmpMessage(bytes_rec) => {
                                    let v = bytes_rec
                                        .get_field_index_for_variant(
                                            lazy_record_type,
                                            field_index,
                                        )?;
                                    stack.push(v.into())?;
                                }
                                BuiltinTypeValue::BmpRouteMonitoringMessage(bytes_rec) => {
                                    let v: TypeValue = LazyRecord::<RouteMonitoring>::new(BytesRecord::<
                                        RouteMonitoring,
                                    >::lazy_type_def(
                                    ))
                                    .get_field_by_index(
                                        field_index,
                                        bytes_rec,
                                    ).map(|elm| elm.try_into())?.map_err(|_| VmError::InvalidPayload)?;

                                    stack.push(v.into())?;
                                }
                                BuiltinTypeValue::BmpInitiationMessage(bytes_rec) => {
                                    let v: TypeValue = LazyRecord::<InitiationMessage>::new(BytesRecord::<
                                        InitiationMessage,
                                    >::lazy_type_def(
                                    ))
                                    .get_field_by_index(
                                        field_index,
                                        bytes_rec,
                                    ).map(|elm| elm.try_into())?.map_err(|_| VmError::InvalidPayload)?;

                                    stack.push(v.into())?;
                                }
                                BuiltinTypeValue::BmpPeerDownNotification(bytes_rec) => {
                                    let v: TypeValue = LazyRecord::<PeerDownNotification>::new(BytesRecord::<
                                        PeerDownNotification,
                                    >::lazy_type_def(
                                    ))
                                    .get_field_by_index(
                                        field_index,
                                        bytes_rec,
                                    ).map(|elm| elm.try_into())?.map_err(|_| VmError::InvalidPayload)?;

                                    stack.push(v.into())?;
                                }
                                BuiltinTypeValue::BmpPeerUpNotification(bytes_rec) => {
                                    let v: TypeValue = LazyRecord::<PeerUpNotification>::new(BytesRecord::<
                                        PeerUpNotification,
                                    >::lazy_type_def(
                                    ))
                                    .get_field_by_index(
                                        field_index,
                                        bytes_rec,
                                    ).map(|elm| elm.try_into())?.map_err(|_| VmError::InvalidPayload)?;

                                    stack.push(v.into())?;
                                }
                                BuiltinTypeValue::BmpTerminationMessage(bytes_rec) => {
                                    let v: TypeValue = LazyRecord::<TerminationMessage>::new(BytesRecord::<
                                        TerminationMessage,
                                    >::lazy_type_def(
                                    ))
                                    .get_field_by_index(
                                        field_index,
                                        bytes_rec,
                                    ).map(|elm| elm.try_into())?.map_err(|_| VmError::InvalidPayload)?;

                                    stack.push(v.into())?;
                                }
                                BuiltinTypeValue::BmpStatisticsReport(bytes_rec) => {
                                    let v: TypeValue = LazyRecord::<StatisticsReport>::new(BytesRecord::<
                                        StatisticsReport,
                                    >::lazy_type_def(
                                    ))
                                    .get_field_by_index(
                                        field_index,
                                        bytes_rec,
                                    ).map(|elm| elm.try_into())?.map_err(|_| VmError::InvalidPayload)?;

                                    stack.push(v.into())?;
                                }
                                _ => {
                                    trace!("Expected Variant, got {:?}", b_tv);
                                    return Err(VmError::InvalidValueType);
                                }
                            }
                        }
                    }
                    // args: [data_source_token, method_token, arguments] The
                    // result of this method will be pushed to the stack.
                    OpCode::ExecuteDataStoreMethod => {
                        if let CommandArg::MemPos(pos) = args.pop()? {
                            *pos as usize
                        } else {
                            return Err(VmError::InvalidValueType);
                        };

                        let _args_len: usize = if let CommandArg::Arguments(
                            args,
                        ) = args.pop()?
                        {
                            args.len()
                        } else {
                            0
                        };

                        trace!(
                            "execute data store method with args {:?}",
                            args
                        );
                        let (method_token, data_source_token) =
                            args.pop_2()?;

                        match data_source_token {
                            CommandArg::DataSourceTable(ds_s)
                            | CommandArg::DataSourceRib(ds_s) => {
                                let ds = self.get_data_source(*ds_s)?;
                                let stack_args = self
                                    ._unwind_resolved_stack_into_vec(mem)?;

                                let v = ds.exec_method(
                                    method_token.try_into()?,
                                    &stack_args[..],
                                    TypeDef::Unknown,
                                )?;
                                let mut s = self.stack.borrow_mut();
                                match v {
                                    DataSourceMethodValue::Ref(sr_pos) => {
                                        s.push(sr_pos)?;
                                    }
                                    DataSourceMethodValue::TypeValue(tv) => {
                                        // mem.set_mem_pos(mem_pos, tv);
                                        s.push(StackRefPos::ConstantValue(
                                            tv,
                                        ))?;
                                    }
                                    DataSourceMethodValue::Empty(_ty) => {
                                        // mem.set_mem_pos(
                                        //     mem_pos,
                                        //     TypeValue::Unknown,
                                        // );
                                        s.push(StackRefPos::ConstantValue(
                                            TypeValue::Unknown,
                                        ))?;
                                    }
                                }
                            }
                            _ => return Err(VmError::InvalidDataSource),
                        }
                    }
                    // stack args: [mem_pos | constant_value]
                    OpCode::PushStack => match args.pop_front() {
                        Some(CommandArg::MemPos(pos)) => {
                            if log_enabled!(Level::Trace) {
                                trace!(
                                    " mem_pos content: {:?}",
                                    mem.get_mem_pos(pos as usize)
                                );
                            }

                            let mut s = self.stack.borrow_mut();
                            s.push(StackRefPos::MemPos(pos))?;
                            if log_enabled!(Level::Trace) {
                                trace!(" stack {:?}", s);
                            }
                        }
                        Some(CommandArg::ConstantIndex(ref c)) => {
                            if log_enabled!(Level::Trace) {
                                trace!(" constant index content: {:?}", c);
                            }
                            let mut s = self.stack.borrow_mut();
                            s.push(StackRefPos::ConstantIndex(
                                c.try_into()?,
                            ))?;
                            if log_enabled!(Level::Trace) {
                                trace!(" stack {:?}", s);
                            }
                        }
                        Some(CommandArg::ConstantValue(v)) => {
                            if log_enabled!(Level::Trace) {
                                trace!(" constant value content: {:?}", v);
                            }
                            let mut s = self.stack.borrow_mut();
                            s.push(StackRefPos::ConstantValue(v))?;
                            if log_enabled!(Level::Trace) {
                                trace!(" stack {:?}", s);
                            }
                        }
                        Some(CommandArg::List(l)) => {
                            if log_enabled!(Level::Trace) {
                                trace!(" list value content: {:?}", l);
                            }
                            let mut s = self.stack.borrow_mut();
                            s.push(StackRefPos::ConstantValue(l.into()))?;
                            if log_enabled!(Level::Trace) {
                                trace!(" stack {:?}", s);
                            }
                        }
                        None => return Err(VmError::InvalidCommand),
                        _ => return Err(VmError::InvalidValueType),
                    },
                    // no stack_args
                    OpCode::PopStack => {
                        if args.is_empty() {
                            let mut s = self.stack.borrow_mut();
                            s.pop()?;
                        } else {
                            return Err(VmError::InvalidValueType);
                        }
                    }
                    // no stack_args
                    OpCode::ClearStack => {
                        let mut s = self.stack.borrow_mut();
                        s.clear();
                    }
                    // stack args: [mem_pos, constant_value]
                    OpCode::MemPosSet => {
                        trace!("memposset value: {:?}", args);

                        if let CommandArg::MemPos(pos) = args[0] {
                            let v = args.take_arg_as_constant()?;
                            mem.set_mem_pos(pos as usize, v);
                            // self.stack.borrow_mut().push(v.into())?;
                        } else {
                            return Err(VmError::InvalidValueType);
                        }
                    }
                    // stack args: [field_index]
                    OpCode::StackOffset => {
                        let mut args = args.args.iter_mut();
                        match args.next() {
                            Some(CommandArg::FieldAccess(_field)) => {
                                for arg in args {
                                    if let CommandArg::FieldAccess(field) =
                                        arg
                                    {
                                        let mut s = self.stack.borrow_mut();
                                        s.add_index_to_field_index(*field)?;
                                    } else {
                                        return Err(
                                            VmError::InvalidValueType,
                                        );
                                    }
                                }
                            }
                            Some(CommandArg::FieldIndex(field_index)) => {
                                let mut s = self.stack.borrow_mut();
                                s.push_with_field_index(std::mem::take(
                                    field_index,
                                ))?;
                            }
                            _ => {
                                return Err(VmError::InvalidValueType);
                            }
                        };
                    }
                    // stack args: [variant_index, variant_index, ..]
                    OpCode::StackIsVariant => {
                        let mut s = self.stack.borrow_mut();

                        for arg in args.args.iter() {
                            if let CommandArg::Variant(variant_index) = arg {
                                match s.get_top_value()?.pos {
                                    // Indexed Variants on Enums only appear
                                    // as MemPos indexes, (rx, tx and
                                    // arguments). We are not doing anything
                                    // to the enum, we are just establishing
                                    // that the enum in this memory position
                                    // holds the requested variant
                                    // corresponding to the index. When it
                                    // does we push it to the stack.
                                    StackRefPos::MemPos(mem_pos) => {
                                        let val = mem.mp_is_variant(
                                            mem_pos as usize,
                                            Token::Variant(*variant_index),
                                        );
                                        trace!(
                                            "stack head is variant {:?}? {:?}",
                                            variant_index, val
                                        );
                                        if val {
                                            let var_val = mem
                                                .get_mp_as_variant_or_unknown(
                                                    mem_pos as usize,
                                                    Token::Variant(
                                                        *variant_index,
                                                    ),
                                                )?;
                                            trace!(
                                                "variant value {:?}",
                                                var_val
                                            );

                                            s.push(StackRefPos::MemPos(
                                                mem_pos,
                                            ))?;
                                        }
                                        s.push(StackRefPos::CompareResult(
                                            val,
                                        ))?;
                                    }
                                    _ => return Err(VmError::InvalidVariant),
                                };
                            }
                        }
                    }
                    // stack args: [arg_token_value]
                    OpCode::PushArgToStack => {
                        trace!("argument {:#?}", self.arguments);
                        match args.get(0) {
                            Some(CommandArg::Argument(token_value)) => {
                                let arg_value = self
                                    .arguments
                                    .get_by_token_value(Token::Argument(
                                        *token_value,
                                    ))
                                    .ok_or_else(|| {
                                        VmError::AnonymousArgumentNotFound
                                    })?;
                                // TODO: THIS SHOULD PROBABLY BE CHANGED TO
                                // USE AN INDEX TO THE arguments map ON THE
                                // VM!
                                self.stack.borrow_mut().push(
                                    StackRefPos::ConstantValue(
                                        arg_value.clone(),
                                    ),
                                )?;
                            }
                            _ => {
                                return Err(VmError::InvalidValueType);
                            }
                        }
                    }
                    OpCode::Label => {
                        // NOOP
                    }
                    // Term procedures
                    // stack args ignored
                    OpCode::SkipToEOB => {
                        break;
                    }
                    // stack args ignored
                    OpCode::CondFalseSkipToEOB => {
                        let s = self.stack.borrow();
                        let stack_ref = s.get_top_value()?;
                        if !mem.get_mp_field_as_bool(stack_ref)? {
                            if log_enabled!(Level::Trace) {
                                trace!(" skip to end of block");
                            }
                            break;
                        } else {
                            if log_enabled!(Level::Trace) {
                                trace!(" continue");
                            }
                            continue;
                        }
                    }
                    // stack args ignored
                    OpCode::CondTrueSkipToEOB => {
                        let s = self.stack.borrow();
                        let stack_ref = s.get_top_value()?;
                        if !mem.get_mp_field_as_bool(stack_ref)? {
                            if log_enabled!(Level::Trace) {
                                trace!(" continue");
                            }
                            continue;
                        } else {
                            if log_enabled!(Level::Trace) {
                                trace!(" skip to end of block");
                            }
                            break;
                        }
                    }
                    OpCode::CondUnknownSkipToLabel => {
                        let s = self.stack.borrow();
                        let stack_ref = s.get_top_value()?;
                        if !mem.get_mp_field_as_unknown(stack_ref) {
                            if log_enabled!(Level::Trace) {
                                trace!(" continue");
                            }
                            continue;
                        } else {
                            if log_enabled!(Level::Trace) {
                                trace!(" skip to next label");
                            }
                            skip_label = true;
                        }
                    }
                    OpCode::CondFalseSkipToLabel => {
                        let s = self.stack.borrow();
                        let stack_ref = s.get_top_value()?;
                        if mem.get_mp_field_as_bool(stack_ref)? {
                            if log_enabled!(Level::Trace) {
                                trace!(" continue");
                            }
                            continue;
                        } else {
                            if log_enabled!(Level::Trace) {
                                trace!(" skip to next label");
                            }
                            skip_label = true;
                        }
                    }
                    // stack args: [exit value]
                    OpCode::Exit(accept_reject) => {
                        // Make sure to TAKE the rx and optionally the tx
                        // value, so the references in LinearMemory are
                        // dropped and LM can be reused.
                        let rx = mem
                            .take_rx_value()
                            .ok_or(VmError::InvalidPayload)?;

                        let tx = match mem.take_tx_value() {
                            Some(TypeValue::Record(rec)) => Some(rec.into()),
                            _ => None,
                        };

                        if *accept_reject != AcceptReject::NoReturn {
                            if log_enabled!(Level::Trace) {
                                trace!("\n\nINITIALIZED MEMORY POSITIONS");
                            }
                            for (i, addr) in
                                mem.0.as_slice().iter().enumerate()
                            {
                                if log_enabled!(Level::Trace)
                                    && !addr.is_unitialized()
                                {
                                    trace!("{}: {}", i, addr);
                                }
                            }

                            trace!(
                                "\n🍺 Done! Successfully executed {} instructions.",
                                commands_num
                            );

                            return Ok(VmResult {
                                accept_reject: *accept_reject,
                                rx,
                                tx,
                                output_stream_queue,
                            });
                        }
                    }

                    // SetRxField will replace the rx instance with the
                    // latest the value that is ref'ed by the top of the
                    // stack.

                    // stack args: [new value]
                    OpCode::SetRxField => {
                        // pop all references from the stack and resolve them
                        // to their values.
                        let stack_args = self.as_vec();

                        // swap out the new value from memory
                        let val = mem.get_mp_field_by_stack_ref_owned(
                            stack_args
                                .last()
                                .ok_or(VmError::StackUnderflow)?,
                        )?;

                        // save the value in memory position 0 (rx instance
                        // by definition).
                        mem.set_mem_pos(0, val);
                    }
                    // stack args: [tx type instance field, new value]
                    OpCode::SetTxField => {
                        todo!();
                    }
                    // stack args: [ordered_field_values..]
                    OpCode::PushOutputStreamQueue => {
                        trace!("Send: {:?}", args);

                        let elem_num =
                            args[2].get_args_len_for_outputstream_record()?;

                        trace!(
                            "no of fields {} elem_num {}",
                            args[3].get_args_len() as u32,
                            elem_num
                        );

                        let stack_args =
                            self._take_resolved_as_owned(elem_num, mem)?;
                        trace!("Stack args {:?}", stack_args);

                        if let CommandArg::MemPos(mem_pos) = args[3] {
                            trace!("from mem pos {}", mem_pos);
                        }

                        let mut rec_fields: Vec<TypeValue> = vec![];

                        for stack_ref in stack_args {
                            rec_fields.insert(0, stack_ref);
                        }

                        if let CommandArg::Arguments(type_def) = &args[2] {
                            trace!("type_def {:?}", type_def[0]);
                            if let TypeDef::OutputStream(os_ty) = &type_def[0]
                            {
                                let rec = Record::create_instance_from_ordered_fields(os_ty, rec_fields)?;
                                output_stream_queue.push(rec.into());
                            }
                        }
                    }
                }
            }

            if log_enabled!(Level::Trace) {
                trace!("\n\n(end) stack: {:?}", self.stack);
                trace!("\nINITIALIZED MEMORY POSITIONS");
            }

            for (i, addr) in mem.0.as_slice().iter().enumerate() {
                if log_enabled!(Level::Trace) && !addr.is_unitialized() {
                    trace!("{}: {}", i, addr);
                }
            }
        }

        // This is obviously not good, so we are terminating here, but first
        // we'll wipe the rx and tx value, so that the LinearMemory instance
        // can still be reused by another VM run.
        mem.reset();
        Err(VmError::UnexpectedTermination)
    }
}

impl<
        MB: AsRef<[MirBlock]> + std::hash::Hash,
        EDS: AsRef<[ExtDataSource]> + std::hash::Hash,
    > std::hash::Hash for VirtualMachine<MB, EDS>
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.mir_code.hash(state);
        self.data_sources.hash(state);
        self.arguments.hash(state);
    }
}

pub struct VmBuilder<MB: AsRef<[MirBlock]>, EDS: AsRef<[ExtDataSource]>> {
    rx_type: TypeDef,
    tx_type: Option<TypeDef>,
    mir_code: Option<MB>,
    arguments: FilterMapArgs,
    data_sources: Option<EDS>,
}

impl<MB: AsRef<[MirBlock]>, EDS: AsRef<[ExtDataSource]>> VmBuilder<MB, EDS> {
    pub fn new() -> Self {
        Self {
            rx_type: TypeDef::default(),
            tx_type: None,
            mir_code: None,
            arguments: FilterMapArgs::default(),
            data_sources: None,
        }
    }

    pub fn with_mir_code(mut self, mir_code: MB) -> Self {
        self.mir_code = Some(mir_code);
        self
    }

    pub fn with_arguments(mut self, args: FilterMapArgs) -> Self {
        self.arguments = args;
        self
    }

    pub fn with_data_sources(mut self, data_sources: EDS) -> Self {
        self.data_sources = Some(data_sources);
        self
    }

    pub fn with_rx(mut self, rx_type: TypeDef) -> Self {
        self.rx_type = rx_type;
        self
    }

    pub fn with_tx(mut self, tx_type: TypeDef) -> Self {
        self.tx_type = Some(tx_type);
        self
    }

    pub fn build(self) -> Result<VirtualMachine<MB, EDS>, VmError> {
        // data sources need to be complete. Check that.
        trace!("data sources in builder");
        let data_sources = if let Some(data_sources) = self.data_sources {
            for ds in data_sources.as_ref().iter() {
                trace!("{}", ds.exists_and_is_empty());
                match ds.exists_and_is_empty() {
                    ExistsAndEmpty(Some(_)) => {}
                    ExistsAndEmpty(None) => {
                        return Err(VmError::DataSourceNotInBuild(
                            ds.get_name(),
                        ))
                    }
                };
            }
            data_sources
        } else {
            return Err(VmError::DataSourcesNotReady);
        };

        if let Some(mir_code) = self.mir_code {
            let hash_id =
                compute_hash(mir_code.as_ref(), data_sources.as_ref());

            Ok(VirtualMachine {
                mir_code,
                data_sources,
                arguments: self.arguments,
                stack: RefCell::new(Stack::new()),
                hash_id,
            })
        } else {
            Err(VmError::NoMir)
        }
    }
}

impl<
        MB: AsRef<[MirBlock]> + std::hash::Hash,
        EDS: AsRef<[ExtDataSource]> + std::hash::Hash,
    > Default for VmBuilder<MB, EDS>
{
    fn default() -> Self {
        Self::new()
    }
}

//------------ VmResult -----------------------------------------------------

#[derive(Debug, Clone)]
pub struct VmResult {
    pub accept_reject: AcceptReject,
    pub rx: TypeValue,
    pub tx: Option<TypeValue>,
    pub output_stream_queue: OutputStreamQueue,
}

//------------ StreamOutputQueue --------------------------------------------

#[derive(Debug, Copy, Clone)]
pub struct StreamId(usize);

#[derive(Debug, Clone)]
pub struct OutputStreamQueue(SmallVec<[OutputStreamMessage; 8]>);

impl OutputStreamQueue {
    pub fn new() -> Self {
        OutputStreamQueue(SmallVec::<[OutputStreamMessage; 8]>::new())
    }

    pub fn push(&mut self, msg: OutputStreamMessage) {
        self.0.push(msg)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn pop(&mut self) -> Option<OutputStreamMessage> {
        self.0.pop()
    }

    pub fn iter(&self) -> impl Iterator<Item = &OutputStreamMessage> + '_ {
        self.0.iter()
    }
}

impl Default for OutputStreamQueue {
    fn default() -> Self {
        Self::new()
    }
}

impl Index<usize> for OutputStreamQueue {
    type Output = OutputStreamMessage;

    fn index(&self, index: usize) -> &Self::Output {
        self.0.index(index)
    }
}

impl IntoIterator for OutputStreamQueue {
    type Item = OutputStreamMessage;
    type IntoIter = <smallvec::SmallVec<[OutputStreamMessage; 8]>
        as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

//------------ VmError ------------------------------------------------------

#[derive(Debug)]
pub enum VmError {
    StackUnderflow,
    StackOverflow,
    MemOutOfBounds,
    InvalidMemoryAccess(usize),
    AnonymousArgumentNotFound,
    ArgumentNotFound(ShortString),
    ArgumentsMissing(Vec<ShortString>),
    InvalidValueType,
    InvalidPayload,
    InvalidVariableAccess,
    InvalidVariant,
    InvalidRecord,
    InvalidFieldAccess,
    InvalidMethodCall,
    DataSourceTokenNotFound(usize),
    DataSourceNotInBuild(ShortString),
    DataSourceEmpty(ShortString),
    InvalidDataSourceAccess,
    DataSourcesNotReady,
    ImpossibleComparison,
    InvalidDataSource,
    InvalidCommand,
    InvalidCommandArg,
    InvalidWrite,
    InvalidConversion,
    InvalidMsgType,
    InvalidCompareOp(usize),
    UnexpectedTermination,
    AsPathTooLong,
    DeltaLocked,
    NoMir,
    ParseError(routecore::bgp::ParseError),
}

impl Display for VmError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            VmError::StackUnderflow => f.write_str("StackUnderflow"),
            VmError::StackOverflow => f.write_str("StackOverflow"),
            VmError::MemOutOfBounds => f.write_str("MemOutOfBounds"),
            VmError::InvalidMemoryAccess(_) => {
                f.write_str("InvalidMemoryAccess")
            }
            VmError::AnonymousArgumentNotFound => {
                f.write_str("AnonymousArgumentNotFound")
            }
            VmError::ArgumentNotFound(_) => f.write_str("ArgumentNotFound"),
            VmError::ArgumentsMissing(_) => f.write_str("ArgumentsMissing"),
            VmError::InvalidValueType => f.write_str("InvalidValueType"),
            VmError::InvalidPayload => f.write_str("InvalidPayload"),
            VmError::InvalidVariableAccess => {
                f.write_str("InvalidVariableAccess")
            }
            VmError::InvalidRecord => f.write_str("InvalidRecord"),
            VmError::InvalidFieldAccess => f.write_str("InvalidFieldAccess"),
            VmError::InvalidMethodCall => f.write_str("InvalidMethodCall"),
            VmError::InvalidDataSourceAccess => {
                f.write_str("InvalidDataSourceAccess")
            }
            VmError::DataSourceTokenNotFound(_) => {
                f.write_str("DataSourceTokenNotFound")
            }
            VmError::DataSourceNotInBuild(ds_name) => {
                write!(
                    f,
                    "Data source '{}' was not in the build arguments.",
                    ds_name
                )
            }
            VmError::DataSourceEmpty(name) => {
                write!(f, "DataSourceEmpty {}", name)
            }
            VmError::DataSourcesNotReady => {
                write!(f, "DataSourceNotReady")
            }
            VmError::ImpossibleComparison => {
                f.write_str("ImpossibleComparison")
            }
            VmError::InvalidWrite => f.write_str("InvalidWrite"),
            VmError::InvalidCommand => f.write_str("InvalidCommand"),
            VmError::InvalidCommandArg => f.write_str("InvalidCommandArg"),
            VmError::InvalidDataSource => f.write_str("InvalidDataSource"),
            VmError::InvalidConversion => f.write_str("InvalidConversion"),
            VmError::InvalidCompareOp(_) => f.write_str("InvalidCompareOp"),
            VmError::UnexpectedTermination => {
                f.write_str("UnexpectedTermination")
            }
            VmError::InvalidMsgType => f.write_str("InvalidMessageType"),
            VmError::InvalidVariant => f.write_str("InvalidVariant"),
            VmError::AsPathTooLong => f.write_str("AsPathTooLong"),
            VmError::DeltaLocked => f.write_str("DeltaLocked"),
            VmError::NoMir => f.write_str("NoMir"),
            VmError::ParseError(e) => {
                let w = write!(f, "{}", e);
                w
            }
        }
    }
}

impl From<VmError> for Box<dyn std::error::Error> {
    fn from(value: VmError) -> Self {
        format!("A fatal VM Error occured: {}", value).into()
    }
}

impl From<routecore::bgp::ParseError> for VmError {
    fn from(value: routecore::bgp::ParseError) -> Self {
        VmError::ParseError(value)
    }
}

#[derive(Debug, Clone)]
pub struct Command {
    pub(crate) op: OpCode,
    pub(crate) args: VecDeque<CommandArg>,
}

impl Command {
    pub fn new(op: OpCode, args: Vec<CommandArg>) -> Self {
        Command {
            op,
            args: args.into(),
        }
    }

    pub fn try_clone(self) -> Result<Self, CompileError> {
        Ok(Self {
            op: self.op,
            args: self.args.clone(),
        })
    }
}

impl std::hash::Hash for Command {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.op.hash(state);
        for a in self.args.iter() {
            a.hash(state);
        }
    }
}

impl Display for Command {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let arrow = match &self.op {
            OpCode::Cmp => "<->",
            OpCode::ExecuteTypeMethod => "->",
            OpCode::ExecuteDataStoreMethod => "->",
            OpCode::ExecuteValueMethod => "->",
            OpCode::ExecuteConsumeValueMethod => "=>",
            OpCode::LoadLazyFieldValue => {
                return write!(f, "💾  {:?} {:?}", self.op, self.args);
            }
            OpCode::PushStack => "<-",
            OpCode::PopStack => "->",
            OpCode::ClearStack => "::",
            OpCode::MemPosSet => "->",
            OpCode::PushArgToStack => "->",
            OpCode::StackOffset => "",
            OpCode::StackIsVariant => {
                return write!(
                    f,
                    "{:?}=={:?}?",
                    self.op,
                    if let Some(arg) = self.args.get(0) {
                        arg.to_string()
                    } else {
                        "(None)".to_string()
                    }
                );
            }
            OpCode::SkipToEOB => "-->",
            OpCode::CondFalseSkipToEOB => "-->",
            OpCode::CondTrueSkipToEOB => "-->",
            OpCode::CondFalseSkipToLabel => "-->",
            OpCode::CondUnknownSkipToLabel => "-->",
            OpCode::Label => {
                return write!(
                    f,
                    "🏷  {}",
                    if let Some(arg) = self.args.get(0) {
                        arg.to_string()
                    } else {
                        "(None)".to_string()
                    }
                );
            }
            OpCode::Exit(accept_reject) => {
                return write!(f, "Exit::{}", accept_reject)
            }
            OpCode::PushOutputStreamQueue => "(o)=>",
            OpCode::SetRxField => "->",
            OpCode::SetTxField => "->",
        };
        write!(f, "{:?}{}{:?}", self.op, arrow, self.args)
    }
}

#[derive(Debug, Hash, Clone)]
pub enum CommandArg {
    // Constant index
    ConstantIndex(TypeValue),
    // TypeValue constant
    ConstantValue(TypeValue),
    // Variable with token value
    Variable(usize),
    // extra runtime argument for filter_map & term
    Argument(usize),
    // a list that needs to be stored at a memory position
    List(List),
    // a record that needs to be stored at a mem position
    Record(Record),
    // the placeholder for the value of the rx type at runtime
    RxValue,
    // the placeholder for the value of the tx type at runtime
    TxValue,
    // method token value
    Method(usize),
    // data source: table token value
    DataSourceTable(usize),
    // data source: rib token value
    DataSourceRib(usize),
    // output stream value
    OutputStream(usize),
    // field access token value
    FieldAccess(usize),
    // field index token value (for LazyRecord)
    FieldIndex(FieldIndex),
    // builtin method token value
    BuiltinMethod(usize),
    // memory position
    MemPos(u32),
    // type definition
    Type(TypeDef),
    // argument types (for method calls)
    Arguments(Vec<TypeDef>),
    // boolean value (used in cmp opcode)
    Boolean(bool),
    // term token value
    Term(usize),
    // compare operation
    CompareOp(ast::CompareOp),
    // a label with its name (to jump to)
    Label(ShortString),
    // argument tell what should happen after
    AcceptReject(AcceptReject),
    // the index of a variant of an enum
    Variant(usize),
}

impl CommandArg {
    pub fn as_token_value(&self) -> Result<usize, VmError> {
        match self {
            CommandArg::Argument(v) => Ok(*v),
            CommandArg::Variable(v) => Ok(*v),
            _ => Err(VmError::InvalidCommandArg),
        }
    }

    pub fn get_args_len(&self) -> usize {
        if let CommandArg::Arguments(args) = self {
            args.len()
        } else {
            1
        }
    }

    pub fn get_args_len_for_outputstream_record(
        &self,
    ) -> Result<u32, VmError> {
        if let CommandArg::Arguments(fields) = &self {
            Ok(fields
                .first()
                .ok_or(VmError::InvalidDataSourceAccess)?
                .get_field_num() as u32)
        } else {
            Ok(0)
        }
    }
}

impl Display for CommandArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CommandArg::Label(v) => write!(f, "{}", v),
            _ => write!(f, ""),
        }
    }
}

impl TryFrom<&CommandArg> for TypeDef {
    type Error = VmError;

    fn try_from(value: &CommandArg) -> Result<Self, VmError> {
        match value {
            CommandArg::Type(t) => Ok(t.clone()),
            _ => {
                error!(
                    "Cannot find TypeDef for command with token: {:?}",
                    value
                );
                Err(VmError::InvalidConversion)
            }
        }
    }
}

// extract the token value from an argument
impl TryFrom<&CommandArg> for usize {
    type Error = VmError;

    fn try_from(value: &CommandArg) -> Result<Self, VmError> {
        match value {
            CommandArg::Method(m) => Ok(*m),
            CommandArg::DataSourceTable(d) => Ok(*d),
            CommandArg::DataSourceRib(d) => Ok(*d),
            CommandArg::FieldAccess(f) => Ok(*f),
            CommandArg::BuiltinMethod(b) => Ok(*b),
            CommandArg::Variable(v) => Ok(*v),
            CommandArg::MemPos(m) => Ok(*m as usize),
            _ => Err(VmError::InvalidCommandArg),
        }
    }
}

impl From<ast::CompareOp> for CommandArg {
    fn from(op: ast::CompareOp) -> Self {
        CommandArg::CompareOp(op)
    }
}

impl TryFrom<crate::traits::Token> for Vec<CommandArg> {
    type Error = VmError;

    fn try_from(to: crate::traits::Token) -> Result<Self, VmError> {
        if let Token::FieldAccess(v) = to {
            Ok(v.iter()
                .map(|f| CommandArg::FieldAccess(*f as usize))
                .collect::<Vec<_>>())
        } else {
            Err(VmError::InvalidFieldAccess)
        }
    }
}

#[derive(Debug, Copy, Clone, Hash)]
pub enum OpCode {
    Cmp,
    ExecuteTypeMethod,
    ExecuteDataStoreMethod,
    ExecuteValueMethod,
    ExecuteConsumeValueMethod,
    // Pop a value from the stack and if it's a LazyRecord or a LazyField on a
    // LazyRecord.
    LoadLazyFieldValue,
    PopStack,
    PushStack,
    ClearStack,
    // Assumes the top of the stack is a Vec of type values, and modifies the
    // value on the top of the stack to the element that is indexed by the
    // first argument is receives. It will recurse into that element if their
    // is a next argument, and repeats that until the arguments are exhausted.
    StackOffset,
    // Inspects the top of the stack to see if its value corresponds to the
    // variant of an enum, passed in as a Variant Token in the arguments of
    // the command. Pushes the result (the boolean) onto the stack. Does not
    // pop the enum instance of the stack.
    StackIsVariant,
    MemPosSet,
    // Push a user-defined argument to the stack.
    PushArgToStack,
    // Conditionally skip to the end ot the MIR block.
    SkipToEOB,
    // Skip to the end of the MIR block if the top of the stack holds a
    // reference to a boolean value true
    CondFalseSkipToEOB,
    // Skip to the end of the MIR block if the top of the stack holds a
    // reference to a boolean value false.
    CondTrueSkipToEOB,
    // Skip to the next label in a MIR block if the top of the stack holds a
    // reference to a TypeValue::Boolean that is false.
    CondFalseSkipToLabel,
    // Skip to the next label in a MIR block if the top of the stack holds a
    // reference to a TypeValue::Unknown. Used to match expression variants.
    CondUnknownSkipToLabel,
    // Debug Label for terms
    Label,
    SetRxField,
    SetTxField,
    // The output stream stack holds indexes to memory positions that contain
    // messages to be send out.
    PushOutputStreamQueue,
    Exit(AcceptReject),
}

pub struct ExtDataSource {
    name: ShortString,
    token: usize,
    ty: TypeDef,
    source: ArcSwapOption<DataSource>,
}

impl std::fmt::Debug for ExtDataSource {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl std::hash::Hash for ExtDataSource {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.token.hash(state);
    }
}
pub struct ExistsAndEmpty(Option<bool>);

impl Display for ExistsAndEmpty {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            ExistsAndEmpty(Some(true)) => {
                write!(f, "source exists and has values")
            }
            ExistsAndEmpty(Some(false)) => {
                write!(f, "source exists, but is empty")
            }
            ExistsAndEmpty(None) => write!(f, "source does not exist"),
        }
    }
}

impl Clone for ExtDataSource {
    fn clone(&self) -> Self {
        Self {
            name: self.name.clone(),
            token: self.token,
            ty: self.ty.clone(),
            source: if let Some(s) = self.source.load().as_ref() {
                ArcSwapOption::from(Some(Arc::clone(s)))
            } else {
                ArcSwapOption::from(None)
            },
        }
    }
}

impl ExtDataSource {
    pub fn new(
        name: &str,
        token: Token,
        ty: TypeDef,
    ) -> Result<ExtDataSource, CompileError> {
        Ok(ExtDataSource {
            name: name.into(),
            token: token.try_into()?,
            source: ArcSwapOption::from(None),
            ty,
        })
    }

    pub fn get_name(&self) -> ShortString {
        self.name.clone()
    }

    pub(crate) fn get_source(&self) -> &ArcSwapOption<DataSource> {
        &self.source
    }

    pub fn get_type(&self) -> &TypeDef {
        &self.ty
    }

    pub fn get_value_type(&self) -> TypeDef {
        match &self.ty {
            TypeDef::Table(t) => *t.clone(),
            TypeDef::Rib((rec, _)) => *rec.clone(),
            TypeDef::OutputStream(s) => *s.clone(),
            _ => self.ty.clone(),
        }
    }

    pub fn exists_and_is_empty(&self) -> ExistsAndEmpty {
        if let Some(source) = self.source.load().as_ref() {
            ExistsAndEmpty(Some(source.as_ref().is_empty()))
        } else {
            ExistsAndEmpty(None)
        }
    }

    pub fn get_at_field_index(
        &self,
        pos: usize,
        field_index: FieldIndex,
    ) -> Result<Option<TypeValue>, VmError> {
        self.source
            .load()
            .as_ref()
            .map(|ds| match ds.get_at_field_index(pos, field_index) {
                Some(TypeValue::SharedValue(sv)) => {
                    Ok(Some(TypeValue::SharedValue(Arc::clone(sv))))
                }
                Some(_) => Err(VmError::InvalidFieldAccess),
                None => Ok(Some(TypeValue::Unknown)),
            })
            .ok_or(VmError::InvalidFieldAccess)?
    }
}

// I'd rather implement SliceIndex on [ExtDataSource], so you could just do
// [..].get() etc, but that's experimental still.
pub fn get_data_source(
    ext_ds: &[ExtDataSource],
    index: Token,
) -> Result<&ExtDataSource, VmError> {
    match index {
        Token::Table(token) => {
            if let Some(s) = ext_ds.get(token) {
                Ok(s)
            } else {
                Err(VmError::InvalidDataSource)
            }
        }
        _ => Err(VmError::InvalidDataSource),
    }
}

impl AsRef<ExtDataSource> for &ExtDataSource {
    fn as_ref(&self) -> &ExtDataSource {
        self
    }
}
