#[feature(macro_rules)];
// #[feature(asm)];

#[link(name="lz4",
    vers="0.1",
    url="https://github.com/MarkJr94/lz4-rs",
    package_id="lz4")];

#[comment = "Rust port of LZ4 algorithm"];
#[license = "MIT"];
#[crate_type = "lib"];

// #[deny(non_camel_case_types)];
#[deny(non_uppercase_statics)];
#[deny(unnecessary_qualification)];
#[warn(missing_doc)];

extern mod extra;

mod macros;
/// This module contains the primary LZ4 interface
pub mod lz4rs;
mod xxhash;

fn main() {
    use xxhash::Xxh32;

    println!("{}", Xxh32::new(0u32).to_str());
}