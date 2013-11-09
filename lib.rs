#[feature(macro_rules)];

#[link(name="lz4",
    vers="0.1",
    url="https://github.com/MarkJr94/lz4-rs")];

#[comment = "Rust port of LZ4 algorithm"];
#[license = "MIT"];
#[crate_type = "lib"];

extern mod extra;

mod macros;
pub mod lz4;
mod xxhash;

if_x64!()
fn main() {
    println!("{}", lz4::ARCH64);
}