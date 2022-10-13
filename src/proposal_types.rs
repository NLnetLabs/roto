use std::{cell::RefCell, rc::Rc};

use routecore::asn::LongSegmentError;

use crate::ast::ShortString;

/// Roto Types
///
/// This module contains the types offered by the Roto languages.

//------------ RFC4271 Route type -------------------------------------------

// routecore, no roto
pub struct Message {
    announcements: Vec<Announcement>,
    withdrawals: Vec<Withdrawal>,
}

// routecore, no roto
pub struct Announcement {
    pub nlri: Nlri,
    pub attributes: BgpAttributes,
    pub afi_safi: (u16, u8),
}

// routecore, no roto
pub struct Withdrawal {
    pub nlri: Nlri,
    pub afi_safi: (u16, u8),
}

// routecore, roto
// Message iterator -> Routes
#[derive(Debug, PartialEq)]
pub struct Route {
    pub prefix: Option<Prefix>,
    pub bgp: Option<BgpAttributes>,
    pub status: Status,
}

#[derive(Debug, PartialEq)]
pub enum Status {
    InConvergence,
    UpToDate,
    Stale,
    StartOfRouteRefresh,
    Withdrawn,
}

// routecore, roto
pub struct RawRoute {
    pub prefix: Option<Prefix>,
    pub message: RawBgpMessage,
    pub status: Status,
}

// routecore, roto
pub struct RawBgpMessage(Vec<u8>);

pub enum Nlri {
    Prefix(Vec<Prefix>),
    FlowSpec(Vec<FlowSpecRule>),
}

pub struct FlowSpecRule {}

#[derive(Debug, PartialEq)]
pub struct BgpAttributes {
    pub as_path: AsPath,
    pub communities: Vec<Community>,
}
