#[macro_escape];

macro_rules! do_while(
    ($body:expr, $cond:expr) => (
        loop {
            $body;

            if !$cond { break; }
        }
    )
)

#[packed]
pub struct U16_S {
    v: u16
}

#[packed]
pub struct U32_S {
    v: u32
}

#[packed]
pub struct U64_S {
    v: u64
}

#[packed]
pub struct SIZE_T {
    v: uint
}

macro_rules! A16(
    ($expr:expr) => (
        {
            let thing: *::macros::U16_S = ::std::cast::transmute($expr);
            (*thing).v
        }
    )
)

macro_rules! A32(
    ($expr:expr) => (
        {
            let thing: *::macros::U32_S = ::std::cast::transmute($expr);
            (*thing).v
        }
    )
)

macro_rules! A64(
    ($expr:expr) => (
        {
            let thing: *::macros::U64_S = ::std::cast::transmute($expr);
            (*thing).v
        }
    )
)

macro_rules! AARCH(
    ($expr:expr) => (
        {
            let thing: *::macros::SIZE_T = ::std::cast::transmute($expr);
            (*thing).v
        }
    )
)