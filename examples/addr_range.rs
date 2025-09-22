use std::net::IpAddr;

use roto::{item, Runtime, Val, Verdict};

#[derive(Clone)]
struct AddrRange {
    min: IpAddr,
    max: IpAddr,
}

fn main() {
    // Create the Roto types to register into the runtime
    let mut ty = item! {
        /// A range of IP addresses
        clone type AddrRange = Val<AddrRange>;
    };

    // Add the contains method to the type.
    ty.add(item! {
        fn contains(range: Val<AddrRange>, addr: IpAddr) -> bool {
            range.min <= addr && addr <= range.max
        }
    });

    // Create the runtime with the type
    let rt = Runtime::from_items(ty).unwrap();

    // Compile the program with our runtime
    let mut program = rt.compile("examples/addr_range.roto").unwrap();

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
