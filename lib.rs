#[feature(macro_rules)];
// #[feature(asm)];

#[link(name="lz4",
    vers="0.1",
    url="https://github.com/MarkJr94/lz4-rs")];

#[comment = "Rust port of LZ4 algorithm"];
#[license = "MIT"];
#[crate_type = "lib"];

extern mod extra;

mod macros;
pub mod lz4rs;
mod xxhash;

fn main() {
    use xxhash::Xxh32;

    println!("{}", Xxh32::new(0u32).to_str());
}