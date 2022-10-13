
// Rotonda types

// THIS IS PSEUDO-CODE!

// This is a file with preliminary proposals for types that are used by one
// or more parts of Rotonda: routecore, rotonda-runtime and roto. Its goal
// is to establish what the contact surface between the different parts of
// Rotonda (including routecore) will (have to) be.

use routecore::asn::LongSegmentError;
use crate::ast::ShortString;

// rotonda-runtime -> routecore -> rotonda-runtime

// One BGP Message as received by rotonda-runtime, then parsed with routecore
pub struct BgpMessage {
    updates: Vec<Update>,
    withdrawals: Vec<Withdrawal>,
}

// routecore -> rotonda-runtime
// An update is a part of a BGP Message that contains an Nlri (most likely
// one or more prefixes). Created by routecore as part of a parsed BGP
// message.
pub struct Update {
    pub nlri: Nlri,
    pub attributes: BgpAttributes,
    pub afi_safi: (u16, u8),
}

// routecore -> rotonda-runtime
// A withdrawal is a BGP message that only has an NLRI, but doesn't have any
// attributes. Created by routecore as part of parsed BGP message.
// Roto doesn't have to deal with this.
pub struct Withdrawal {
    pub nlri: Nlri,
    pub afi_safi: (u16, u8),
}

// routecore -> rotonda-runtime -> roto

// A Route according to RFC4271, a record of the form (prefix, bgp_attributes)
// with some added metadata, that is strictly speaking not part of a RFC4271
// route.

// The RFC (https://tools.ietf.org/html/rfc4271#section-1.1):

// ```
// Route
// A unit of information that pairs a set of destinations with the
// attributes of a path to those destinations.  The set of
// destinations are systems whose IP addresses are contained in one
// IP address prefix carried in the Network Layer Reachability
// Information (NLRI) field of an UPDATE message.  The path is the
// information reported in the path attributes field of the same
// UPDATE message.
// ```

// Message iterator -> Routes
#[derive(Debug, PartialEq)]
pub struct Route {
    pub prefix: Option<Prefix>,
    pub bgp: Option<BgpAttributes>,
    pub status: Status,
}

// routecore -> rotonda-runtime -> roto

// Status is piece of metadata that writes some (hopefully) relevant state of
// per-peer BGP session into every route. The goal is to be able to enable the
// logic in `rib-units` to decide whether routes should be send to its output
// and to be able output this information to API clients, without having to
// go back to the units that keep the per-peer session state.
#[derive(Debug, PartialEq)]
pub enum Status {
    InConvergence,       // Between start and EOR on a BGP peer-session
    UpToDate,            // After EOR for a BGP peer-session, either 
                         // `Graceful Restart` or EOR
    Stale,               // After hold-timer expiry
    StartOfRouteRefresh, // After the request for a Route Refresh to a peer
                         // and the reception of a new route
    Withdrawn,           // After the reception of a withdrawal
    Empty,               // Status not relevant, e.g. a RIB that holds
                         // archived routes.
}

// rotonda-runtime

// A BGP message that will be stored as a array of bytes (whatever Rust data-
// structures that may result in), that can be stored in a RIB, most likely
// the RIB that lives directy next to an stream-connector. Can be parsed
// on-the-fly when needed, and thus roto doesn't have to have any knowledge
// of this type.
pub struct RawRoute {
    pub prefix: Option<Prefix>,
    pub message: RawBgpMessage,
    pub status: Status,
}

// rotonda-runtime

// A data-structure that stores the array of bytes. Could be somethin else as
// well, but it should be usable by `OctSeq`.
pub struct RawBgpMessage(Vec<u8>);


// routecore -> rotonda-runtime

// An NLRI is the part of a BGP message that lives in the NLRI section, but
// it can also be the MP_REACH_NLRI and MP_UNREACH_NLRI in RFC4760.
// In most cases it will be one or more prefixes, but it can also be. for
// example, a FlowSpec rule.
pub enum Nlri {
    Prefix(Vec<Prefix>),
    FlowSpec(Vec<FlowSpecRule>),
}

// routecore -> rotonda-runtime -> roto 
pub struct FlowSpecRule {}

// routecore -> rotonda-runtime -> roto 

// The record that holds all the BGP attributes in a BGP message to be used
// by roto and all other parts of Rotonda.
#[derive(Debug, PartialEq)]
pub struct BgpAttributes {
    pub as_path: AsPath,
    pub origin: Origin,
    pub next_hop: Option<IpAddr>,
    pub multi_exit_disc: Option<u32>,
    pub local_pref: Option<u32>,
    pub atomic_aggregate: bool,
    pub aggregator: Option<Aggregator>,
    pub communities: Vec<Community>,
    // pub originator_id: Option<OriginatorId>,
    // pub cluster_list: Vec<ClusterId>,
    // pub pmsi_tunnel: Option<PmsiTunnel>,
    // pub tunnel_encapsulation: Option<TunnelEncapsulation>,
    // pub traffic_engineering: Option<TrafficEngineering>,
    // pub aigp: Option<Aigp>,
    // pub pe_distinguisher_labels: Vec<PeDistinguisherLabel>,
    // pub bgp_ls: Option<BgpLs>,
    // pub bgpsec_path: Option<BgpsecPath>,
    // pub sfp: Option<Sfp>,
    // pub bfd_discriminator: Option<BfdDiscriminator>,
    // pub bgp_prefix_sid,
    // pub attr_set: Option<AttrSet>,
    pub unknown: Vec<UnknownAttribute>,
}
