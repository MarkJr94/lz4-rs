use std::ptr;

macro_rules! A32(
    ($expr:expr) => (
        {
            let thing: *u32 = ::std::cast::transmute($expr);
            *thing
        }
    )
)

macro_rules! rotl32(
    ($x:expr, $r:expr) => ( ($x << $r) | ($x >> (32 - $r)) )
)

// macro_rules! rotl32u(
//     ($x:expr, $r:expr) => ( (x << r) | (x >> (32u32 - r)) )
// )

macro_rules! swap32(
    ($x:expr) => (::std::unstable::intrinsics::bswap32($x as i32))
)

macro_rules! do_while(
    ($body:expr, $cond:expr) => (
        loop {
            $body;

            if !$cond { break; }
        }
    )
)

static SIZE_OF_STATE: uint = 48;

#[cfg(target_endian = "big")]
static ENDIANNESS: Endianness =  BigEndian;

#[cfg(target_endian = "little")]
static ENDIANNESS: Endianness =  LittleEndian;

// pub fn size_of_state() -> uint {
//     std::sys::size_of::<State>()
// }

#[deriving(Eq, Clone, ToStr)]
enum ErrCode {
    Okay = 0,
    Error
}

#[deriving(Eq, Clone, ToStr)]
enum Alignment {
    Aligned,
    Unaligned
}

#[deriving(Eq, Clone, ToStr)]
enum Endianness {
    LittleEndian,
    BigEndian
}

static PRIME32_1: u32 = 2654435761u32;
static PRIME32_2: u32 = 2246822519u32;
static PRIME32_3: u32 = 3266489917u32;
static PRIME32_4: u32 = 668265263u32;
static PRIME32_5: u32 = 374761393u32;

#[inline(always)]
fn read_le32_align(ptr: *u32, endian: Endianness, align: Alignment) -> u32 {
    unsafe {
        if align == Unaligned {
            if endian == LittleEndian { A32!(ptr) } else { (swap32!(A32!(ptr)) as u32) }
        } else {
            if endian == LittleEndian { *ptr } else { (swap32!(*ptr) as u32) }
        }
    }
}

#[inline(always)]
fn read_le32(ptr: *u32, endian: Endianness) -> u32 {
    read_le32_align(ptr, endian, Unaligned)
}

#[inline(always)]
fn endian_align(input: *u8, len: uint, seed: u32, endian: Endianness, align: Alignment) -> u32 {
    let mut p = input;
    let b_end = unsafe { input.offset(len as int) };
    let mut h32: u32;

    if len >= 16u {
        let limit = unsafe { b_end.offset(-16) };
        let mut v1: u32 = seed + PRIME32_1 + PRIME32_2;
        let mut v2: u32 = seed + PRIME32_2;
        let mut v3: u32 = seed + 0u32;
        let mut v4: u32 = seed - PRIME32_1;

        do_while!({
            v1 += read_le32_align(p as *u32, endian, align) * PRIME32_2;
            v1 = rotl32!(v1, 13);
            v1 *= PRIME32_1;
            p = unsafe { p.offset(4) };

            v2 += read_le32_align(p as *u32, endian, align) * PRIME32_2;
            v2 = rotl32!(v2, 13);
            v2 *= PRIME32_1;
            p = unsafe { p.offset(4) };

            v3 += read_le32_align(p as *u32, endian, align) * PRIME32_2;
            v3 = rotl32!(v3, 13);
            v3 *= PRIME32_1;
            p = unsafe { p.offset(4) };

            v4 += read_le32_align(p as *u32, endian, align) * PRIME32_2;
            v4 = rotl32!(v4, 13);
            v4 *= PRIME32_1;
            p = unsafe { p.offset(4) };

        }, p <= limit)

        h32 = rotl32!(v1, 1) + rotl32!(v2, 7) + rotl32!(v3, 12) + rotl32!(v4, 18);
    } else {
        h32 = seed + PRIME32_5;
    }

    h32 += len as u32;

    while unsafe { p <= b_end.offset(-4) } {
        h32 += read_le32_align(p as *u32, endian, align) * PRIME32_3;
        h32 = rotl32!(h32, 17) * PRIME32_4;
        p = unsafe { p.offset(4) };
    }

    while p <= b_end {
        h32 += unsafe { *p } as u32 * PRIME32_5;
        h32 = rotl32!(h32, 11) * PRIME32_1;
        p = unsafe { p.offset(1) };
    }

    h32 ^= h32 >> 15;
    h32 *= PRIME32_2;
    h32 ^= h32 >> 13;
    h32 *= PRIME32_3;
    h32 ^= h32 >> 16;

    h32
}

fn xxh32(input: *u8, len: uint, seed: u32) -> u32 {
    if ENDIANNESS == LittleEndian {
        endian_align(input, len, seed, LittleEndian, Unaligned)
    } else {
        endian_align(input, len, seed, BigEndian, Unaligned)
    }
}

// #[deriving(Clone, ToStr)]
struct State32 {
    total_len: u64,
    seed: u32,
    v1: u32,
    v2: u32,
    v3: u32,
    v4: u32,
    memsize: i32,
    memory: [u8, ..16]
}

impl State32 {
    pub fn new(seed: u32) -> State32 {
        State32 {
            total_len: 0u64,
            seed: seed,
            v1: seed + PRIME32_1 + PRIME32_2,
            v2: seed + PRIME32_2,
            v3: seed + 0u32,
            v4: seed - PRIME32_1,
            memsize: 0i32,
            memory: [0u8, ..16]
        }
    }
}

fn size_of_state() -> uint {
    use std::mem;

    assert!(SIZE_OF_STATE >= mem::size_of::<State32>());

    mem::size_of::<State32>()
}

fn reset_state(state: &mut State32, seed: u32) -> ErrCode {
    state.seed = seed;
    state.v1 = seed + PRIME32_1 + PRIME32_2;
    state.v2 = seed + PRIME32_2;
    state.v3 = seed + 0u32;
    state.v4 = seed + PRIME32_1;
    state.total_len = 0u64;
    state.memsize = 0i32;
    Okay
}

#[inline(always)]
fn update_endian(state: &mut State32, input: *u8, len: uint, endian: Endianness) -> ErrCode {
    let mut p = input;
    let b_end = unsafe { input.offset(len as int) };

    assert!(ptr::is_not_null(p));

    state.total_len += len as u64;

    if state.memsize + (len as i32) < 16 {
        do state.memory.as_mut_buf |buf, _| {
            unsafe {
                ptr::copy_memory(buf.offset(state.memsize as int), input, len);
            }
        };

        state.memsize += len as i32;
        return Okay;
    }

    if (state.memsize > 0) {
        do state.memory.as_mut_buf |buf, _| {
            unsafe {
                ptr::copy_memory(buf.offset(state.memsize as int), input, 16 - state.memsize as uint);
            }
        };
        unsafe {
            let mut p32: *u32 = p as *u32;
            state.v1 += read_le32(p32, endian) * PRIME32_2;
            state.v1 = rotl32!(state.v1, 13);
            state.v1 *= PRIME32_1;
            p32 = p32.offset(1);

            state.v2 += read_le32(p32, endian) * PRIME32_2;
            state.v2 = rotl32!(state.v2, 13);
            state.v2 *= PRIME32_1;
            p32 = p32.offset(1);

            state.v3 += read_le32(p32, endian) * PRIME32_2;
            state.v3 = rotl32!(state.v3, 13);
            state.v3 *= PRIME32_1;
            p32 = p32.offset(1);

            state.v4 += read_le32(p32, endian) * PRIME32_2;
            state.v4 = rotl32!(state.v4, 13);
            state.v4 *= PRIME32_1;
//             p32 = p32.offset(1);
        }

        p = unsafe { p.offset(16 - state.memsize as int) };
        state.memsize = 0i32;
    }

    if unsafe { p <= b_end.offset(-16) }{
        let limit = unsafe { b_end.offset(-16) };
        let mut v1 = state.v1;
        let mut v2 = state.v2;
        let mut v3 = state.v3;
        let mut v4 = state.v4;

        do_while!({
            v1 += read_le32(p as *u32, endian) * PRIME32_2;
            v1 = rotl32!(v1, 13);
            v1 *= PRIME32_1;
            p = unsafe { p.offset(4) };

            v2 += read_le32(p as *u32, endian) * PRIME32_2;
            v2 = rotl32!(v2, 13);
            v2 *= PRIME32_1;
            p = unsafe { p.offset(4) };

            v3 += read_le32(p as *u32, endian) * PRIME32_2;
            v3 = rotl32!(v3, 13);
            v3 *= PRIME32_1;
            p = unsafe { p.offset(4) };

            v4 += read_le32(p as *u32, endian) * PRIME32_2;
            v4 = rotl32!(v4, 13);
            v4 *= PRIME32_1;
            p = unsafe { p.offset(4) };

        }, p <= limit)

        state.v1 = v1;
        state.v2 = v2;
        state.v3 = v3;
        state.v4 = v4;
    }

    if p < b_end {
        do state.memory.as_mut_buf |buf, _| {
            unsafe {
                ptr::copy_memory(buf, p, b_end.to_uint() - p.to_uint());
            }
        };

        state.memsize = (b_end.to_uint() - p.to_uint() ) as i32;
    }

    Okay
}