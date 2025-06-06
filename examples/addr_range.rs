use std::{net::IpAddr, path::Path};

use roto::{roto_method, FileTree, Runtime, Val, Verdict};

#[derive(Clone)]
struct AddrRange {
    min: IpAddr,
    max: IpAddr,
}

fn main() {
    let path = Path::new("./examples/addr_range.roto");

    // Create a runtime
    let mut runtime = Runtime::new();

    // Register the AddrRange type into Roto with a docstring
    runtime
        .register_clone_type::<AddrRange>("A range of IP addresses")
        .unwrap();

    // Register the contains method with a docstring
    #[roto_method(runtime, AddrRange)]
    fn contains(range: Val<AddrRange>, addr: Val<IpAddr>) -> bool {
        range.min <= *addr && *addr <= range.max
    }

    // Compile the program with our runtime
    let mut program = FileTree::read(path).compile(runtime).unwrap();

    // Extract the function
    let function = program
        .get_function::<(), fn(Val<AddrRange>, IpAddr) -> Verdict<IpAddr, ()>>(
            "within_range",
        )
        .unwrap();

    let range = AddrRange {
        min: "10.10.10.10".parse().unwrap(),
        max: "10.10.10.12".parse().unwrap(),
    };

    // Run the function
    let in_range = "10.10.10.11".parse().unwrap();
    println!("{:?}", function.call(&mut (), Val(range.clone()), in_range));

    let out_of_range = "20.20.20.20".parse().unwrap();
    println!("{:?}", function.call(&mut (), Val(range), out_of_range));
}
