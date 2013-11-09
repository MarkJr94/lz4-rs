#[macro_escape];

macro_rules! if_x64(
    () => ( #[cfg(target_word_size = "64")])
)

macro_rules! if_big_endian(
    () => (#[cfg(target_endian = "big")])
)

macro_rules! if_little_endian(
    () => (#[cfg(target_endian = "little")])
)

macro_rules! expect(
    ($expr:expr, $value:expr) => ( expr )
)

macro_rules! likely(
    ($expr:expr) => ( expect!($expr == true, true))
)

macro_rules! unlikely(
    ($expr:expr) => ( expect!($expr == true, false))
)