use std::unstable::intrinsics;

//**************************************
// Tuning parameters
//**************************************
// MEMORY_USAGE :
// Memory usage formula : N->2^N Bytes (examples : 10 -> 1KB; 12 -> 4KB ; 16 -> 64KB; 20 -> 1MB; etc.)
// Increasing memory usage improves compression ratio
// Reduced memory usage can improve speed, due to cache effect
// Default value is 14, for 16KB, which nicely fits into Intel x86 L1 cache
static MEMORY_USAGE: uint = 14;


// HEAPMODE :
// Select how default compression functions will allocate memory for their hash table,
// in memory stack (0:default, fastest), or in memory heap (1:requires memory allocation (malloc)).
static HEAPMODE: bool = false;

#[cfg(target_word_size = "64")]
static ARCH64: bool = true;

#[cfg(target_word_size = "32")]
static ARCH64: bool = false;

#[cfg(target_endian = "big")]
static BIG_ENDIAN: bool = true;

#[cfg(target_endian = "little")]
static BIG_ENDIAN: bool = false;


// Constants ======================================
// ================================================
static HASHLOG: uint = MEMORY_USAGE - 2;
static HASH_TABLE_SIZE: uint = 1 << MEMORY_USAGE;
static HASHNBCELLS4: uint = 1 << HASHLOG;

static MINMATCH: uint = 4;

static COPY_LENGTH: uint = 8;
static LAST_LITERALS: uint = 5;
static MFLIMIT: uint = COPY_LENGTH + MINMATCH;
static MINLENGTH: uint = MFLIMIT + 1;

static LIMIT64K: uint = (1 << 16) + (MFLIMIT - 1);
static SKIP_STRENGTH: uint = 6;

static MAXD_LOG: uint = 16;
static MAX_DISTANCE: uint = (1 << MAXD_LOG) - 1;

static ML_BITS: uint = 4;
static ML_MASK: uint = (1<< ML_BITS) - 1;
static RUN_BITS: uint = 8 - ML_BITS;
static RUN_MASK: uint = (1 << RUN_BITS) - 1;

// Macros =========================================
// ================================================
macro_rules! KB(
    ($expr:expr) => ($expr * (1u << 10))
)

macro_rules! MB(
    ($expr:expr) => ($expr * (1u << 20))
)

macro_rules! GB(
    ($expr:expr) => ($expr * (1u << 30))
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

macro_rules! swap16(
    ($x:expr) => (::std::unstable::intrinsics::bswap16($x as i16))
)

// Structs and local types ========================
// ================================================
pub struct Lz4Data {
    hash_table: [u32, ..HASHNBCELLS4],
    buffer_start: *u8,
    base: *u8,
    next_block: *u8
}

#[deriving(Eq, ToStr, Clone)]
enum LimitiedOutput {
    NotLimited,
    Limited
}

#[deriving(Eq, ToStr, Clone)]
enum TableType {
    ByPtr,
    ByU32,
    ByU16
}

#[deriving(Eq, ToStr, Clone)]
enum Prefix64k {
    NoPrefix,
    WithPrefix
}

#[deriving(Eq, ToStr, Clone)]
enum EndCondition {
    OnOutPutSize,
    OnInputSize
}

#[deriving(Eq, ToStr, Clone)]
enum EarlyEnd {
    Full,
    Partial
}

// Architecture-related macros/constants ==========
// ================================================
static STEPSIZE: uint = ::std::uint::bytes;

macro_rules! copy_step(
    ($d:expr, $s:expr) => (
        { AARCH!($d) = AARCH!($s); d.offset(STEPSIZE as int); s.offset(STEPSIZE as int); }
    )
)

macro_rules! copy8(
    ($d:expr, $s:expr) => (
        { copy_step!($d, $s); if STEPSIZE < 8 { copy_step!($d, $s) } }
    )
)

macro_rules! wild_copy(
    ($d:expr, $s:expr, $e:expr) => (
        { do_while!(copy8!($d, $s), $d < $e) }
    )
)

macro_rules! secure_copy(
    ($d:expr, $s:expr, $e:expr) => (
        { if STEPSIZE == 4 || $d < $e { wild_copy!($d, $s, $e) } }
    )
)

#[cfg(target_word_size = "64")]
type HTYPE = u32;

#[cfg(target_word_size = "64")]
macro_rules! init_base(
    ($base:ident) => (
        let $base: *u8 =  ip;
    )
)

#[cfg(target_word_size = "32")]
type HTYPE = *u64;

#[cfg(target_word_size = "32")]
macro_rules! init_base(
    ($base:ident) => (
        let $base: int =  0;
    )
)

#[cfg(target_endian = "big")]
macro_rules! read_le16(
    ($d:expr, $s:expr, $p:expr) => ( { let v = A16!($p); v = bswap16!(v); $d = $s - v; } )
)

#[cfg(target_endian = "big")]
macro_rules! write_le16(
    ($p:expr, $i:expr) => ( { let v = $i as u16;  v = bswap16!(v); *($p as *u16) = v; $p = $p.offset(2); } )
)

#[cfg(target_endian = "little")]
macro_rules! read_le16(
    ($d:expr, $s:expr, $p:expr) => ( $d = $s - A16!($p); )
)

#[cfg(target_endian = "little")]
macro_rules! read_le16(
    ($p:expr, $v:expr) => ( { *($p as *u16) = $v; $p.offset(2); } )
)


// Private functions ==============================
// ================================================
#[cfg(target_endian = "big", target_word_size = "64")]
fn nb_common_bytes(val: u64) -> int {
    unsafe { (intrinsics::ctlz64(val as i64) >> 3) as int }
}

#[cfg(target_endian = "little", target_word_size = "64")]
fn nb_common_bytes(val: u64) -> int {
    unsafe { (intrinsics::cttz64(val as i64) >> 3) as int }
}

#[cfg(target_endian = "big", target_word_size = "32")]
fn nb_common_bytes(val: u32) -> int {
    unsafe { (intrinsics::ctlz32(val as i32) >> 3) as int }
}

#[cfg(target_endian = "little", target_word_size = "32")]
fn nb_common_bytes(val: u32) -> int {
    unsafe { (intrinsics::cttz32(val as i32) >> 3) as int }
}

// Compression functions ==========================
// ================================================
fn hash_sequence(sequence: u32, table_type: TableType) -> int {
    let ret = if table_type == ByU16 {
        ((sequence) *  2654435761u32) >> ((MINMATCH*8) - (HASHLOG+1))
    } else {
        ((sequence) * 2654435761u32) >> ((MINMATCH*8) - HASHLOG)
    } as int;
    ret
}