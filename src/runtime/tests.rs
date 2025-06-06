use super::Runtime;
use roto_macros::{roto_function, roto_method, roto_static_method};
use routecore::bgp::{
    aspath::{AsPath, HopPath},
    communities::{Community, Wellknown},
    types::{LocalPref, OriginType},
};

pub fn routecore_runtime() -> Result<Runtime, String> {
    let mut rt = Runtime::new();

    rt.register_clone_type::<OriginType>("TODO")?;
    rt.register_clone_type::<LocalPref>("TODO")?;
    rt.register_clone_type::<Community>("TODO")?;
    rt.register_clone_type::<HopPath>("TODO")?;
    rt.register_clone_type::<AsPath<Vec<u8>>>("TODO")?;

    #[roto_function(rt)]
    fn pow(x: u32, y: u32) -> u32 {
        x.pow(y)
    }

    #[roto_method(rt, u32)]
    fn is_even(x: u32) -> bool {
        x % 2 == 0
    }

    rt.register_constant(
        "BLACKHOLE",
        "The well-known BLACKHOLE community.",
        Community::from(Wellknown::Blackhole),
    )
    .unwrap();

    Ok(rt)
}

#[test]
pub fn default_runtime() {
    let rt = routecore_runtime().unwrap();

    let names: Vec<_> = rt.runtime_types.iter().map(|ty| &ty.name).collect();
    assert_eq!(
        names,
        &[
            "Unit",
            "bool",
            "u8",
            "u16",
            "u32",
            "u64",
            "i8",
            "i16",
            "i32",
            "i64",
            "f32",
            "f64",
            "Asn",
            "IpAddr",
            "Prefix",
            "String",
            "OriginType",
            "LocalPref",
            "Community",
            "HopPath",
            "AsPath"
        ]
    );
}

#[test]
fn issue_54() {
    let mut rt = Runtime::new();

    struct Foo {
        _x: i32,
    }
    extern "C" fn bar(_foo: *mut Foo, _x: u32) {}

    // We 'forget' to register type Foo:
    // rt.register_type::<Foo>().unwrap();

    // But we do register a method on it:
    rt.register_method::<Foo, _, _>("bar", bar as extern "C" fn(_, _) -> _)
        .unwrap_err();
}

#[test]
#[should_panic]
fn invalid_function_name() {
    let mut rt = Runtime::new();

    #[roto_function(rt)]
    fn accept() -> bool {
        true
    }
}

#[test]
#[should_panic]
fn invalid_method_name() {
    let mut rt = Runtime::new();

    #[roto_method(rt, bool)]
    fn accept(_x: bool) -> bool {
        true
    }
}
#[test]
#[should_panic]
fn invalid_static_method_name() {
    let mut rt = Runtime::new();

    #[roto_static_method(rt, bool)]
    fn accept() -> bool {
        true
    }
}

#[test]
fn invalid_type_name() {
    let mut rt = Runtime::new();

    #[allow(non_camel_case_types)]
    #[derive(Clone, Copy)]
    struct accept;

    rt.register_clone_type::<accept>("").unwrap_err();
    rt.register_copy_type::<accept>("").unwrap_err();
}
