use std::error::Error;

use inetnum::addr::Prefix;
use inetnum::asn::Asn;
use roto::{roto_method, TypedFunc, Val, Verdict};
use routecore::bgp::aspath::{Hop, HopPath};
use routecore::bgp::nlri::afisafi::IsPrefix;
use routecore::bgp::workshop::route::RouteWorkshop;

mod hidden {
    use inetnum::addr::Prefix;
    use routecore::bgp::{
        nlri::afisafi::{
            Ipv4MulticastNlri, Ipv4UnicastNlri, Ipv6MulticastNlri,
            Ipv6UnicastNlri,
        },
        path_attributes::PaMap,
        workshop::route::RouteWorkshop,
    };

    #[allow(dead_code)]
    #[derive(Clone)]
    pub enum RotondaRoute {
        Ipv4Unicast(RouteWorkshop<Ipv4UnicastNlri>),
        Ipv6Unicast(RouteWorkshop<Ipv6UnicastNlri>),
        Ipv4Multicast(RouteWorkshop<Ipv4MulticastNlri>),
        Ipv6Multicast(RouteWorkshop<Ipv6MulticastNlri>),
    }

    impl RotondaRoute {
        pub fn attributes(&self) -> &PaMap {
            match self {
                RotondaRoute::Ipv4Unicast(n) => n.attributes(),
                RotondaRoute::Ipv6Unicast(n) => n.attributes(),
                RotondaRoute::Ipv4Multicast(n) => n.attributes(),
                RotondaRoute::Ipv6Multicast(n) => n.attributes(),
            }
        }
    }

    #[derive(Debug)]
    pub struct OutputStream<T>(Vec<T>);

    impl<T> Default for OutputStream<T> {
        fn default() -> Self {
            Self(Vec::default())
        }
    }

    impl<T> OutputStream<T> {
        pub fn push(&mut self, value: T) {
            self.0.push(value)
        }

        pub fn drain(&mut self) -> std::vec::Drain<'_, T> {
            self.0.drain(..)
        }
    }

    #[derive(Debug)]
    #[allow(dead_code)]
    pub enum Output {
        Custom(u32, u32),
        Prefix(Prefix),
    }
}

use hidden::*;

type Log = *mut OutputStream<Output>;
type Func = TypedFunc<(), fn(Val<Log>, Val<RotondaRoute>) -> Verdict<(), ()>>;

fn main() -> Result<(), Box<dyn Error>> {
    let mut rt = roto::Runtime::new();

    // Registering types and their methods

    rt.register_clone_type_with_name::<RotondaRoute>("Route", "A route")?;

    rt.register_clone_type_with_name::<Log>("Log", "A thing to log to")?;

    #[roto_method(rt, RotondaRoute)]
    fn prefix_matches(rr: Val<RotondaRoute>, to_match: Val<Prefix>) -> bool {
        let rr_prefix = match &*rr {
            RotondaRoute::Ipv4Unicast(n) => n.nlri().prefix(),
            RotondaRoute::Ipv6Unicast(n) => n.nlri().prefix(),
            RotondaRoute::Ipv4Multicast(n) => n.nlri().prefix(),
            RotondaRoute::Ipv6Multicast(n) => n.nlri().prefix(),
        };

        rr_prefix == *to_match
    }

    #[allow(improper_ctypes_definitions)] // While Asn in inetnum is not FFI-safe
    #[roto_method(rt, RotondaRoute, aspath_origin)]
    fn rr_aspath_origin(rr: Val<RotondaRoute>, to_match: Asn) -> bool {
        if let Some(hoppath) = rr.attributes().get::<HopPath>() {
            if let Some(Hop::Asn(asn)) = hoppath.origin() {
                return *asn == to_match;
            }
        }

        false
    }

    #[roto_method(rt, Log)]
    fn log_prefix(mut stream: Val<Log>, prefix: Val<Prefix>) {
        let stream = unsafe { &mut **stream };
        stream.push(Output::Prefix(*prefix));
    }

    #[roto_method(rt, Log)]
    fn log_custom(mut stream: Val<Log>, id: u32, local: u32) {
        let stream = unsafe { &mut **stream };
        stream.push(Output::Custom(id, local));
    }

    let mut compiled = rt
        .compile("examples/presentation.roto")
        .inspect_err(|e| eprintln!("{e}"))?;

    let function = compiled.get_function("rib_in_pre").unwrap();

    run_with_prefix(&function, "8.8.8.0/24")?;
    run_with_prefix(&function, "100.40.0.0/17")?;

    Ok(())
}

fn run_with_prefix(
    function: &Func,
    prefix: &str,
) -> Result<(), Box<dyn Error>> {
    let route =
        RotondaRoute::Ipv4Unicast(RouteWorkshop::new(prefix.parse()?));
    let mut output = OutputStream::default();
    let log = &mut output as *mut _;

    let verdict = function.call(&mut (), Val(log), Val(route));

    println!("Input: {prefix}");
    println!("Verdict: {verdict:?}");
    println!();
    println!("Output: ");
    for entry in output.drain() {
        println!(" - {entry:?}");
    }
    println!("=====================");

    Ok(())
}
