use std::unstable::intrinsics;
use std::ptr;

// ****************************
// Simple Functions
// ****************************


/// Compresses input to destination in one shot
/// # Arguments
/// * `source` - data to be compressed
/// * `dest` - output buffer for compressed data
/// # Return Values
/// returns the number of bytes compressed, or 0 if failure
/// # Safety Notes
/// `dest` must be large enough for worst-case compression (not at all)
///     to get this information, use `compress_bound(source.len())`
pub fn compress(source: &[u8], dest: &mut[u8]) -> int {
    use std::mem::size_of;

    let ctx = &mut [0u32, ..(1u << (MEMORY_USAGE - 2))];

    do ctx.as_mut_buf |ctx_p, _| {
        do source.as_imm_buf |src, src_size| {
            do dest.as_mut_buf |dst, _ | {
                unsafe {
                    if src_size < LIMIT64K {
                        compress_generic(ctx_p as *mut Lz4Data, src, dst, src_size, 0,
                            NotLimited, ByU16, NoPrefix)
                    } else {
                        compress_generic(ctx_p as *mut Lz4Data, src, dst, src_size, 0, NotLimited,
                            if size_of::<*()>() == 8 { ByU32 } else { ByPtr }, NoPrefix)
                    }
                }
            }
        }
    }
}

/// Decompresses `source` to `dest` safely, such that it never reads outside of the input buffer.
/// # Arguments
/// * `source` - data to be decompressed
/// * `dest` - destination buffer
/// # Return values
/// Returns the the number of bytes decoded in the `dest` buffer. (necessarily <= dest.len()) or
///     negative value on malformed input
pub fn decompress_safe(source: &[u8], dest: &mut[u8]) -> int {
    do source.as_imm_buf | src, src_len | {
        do dest.as_mut_buf | dst, dst_len | {
            unsafe {
                decompress_generic(src, dst, src_len, dst_len, OnInputSize, NoPrefix, Full, 0)
            }
        }
    }
}

//****************************
// Advanced Functions
//****************************

/// Returns the worst case output size of a compression attempt
/// # Arguments
/// * `isize` - the size of the input in bytes
/// # Return value
/// The worst case size of the compressed output, or None if input is too large
#[inline]
pub fn compress_bound(isize: uint) -> Option<uint> {
    if isize > MAX_INPUT_SIZE { None } else { Some(isize + ((isize / 50) + 16)) }
}

/// Compresses input to destination in one shot, with limit room for output
/// Max supported value is `MAX_INPUT_SIZE`
/// # Arguments
/// * `source` - data to be compressed
/// * `dest` - output buffer for compressed data
/// * `max_out_size` - maximum amount of data that may be written to `dest`
/// # Return Values
/// returns the number of bytes compressed, or 0 for failure or lack of room in `dest`
pub fn compress_limited(source: &[u8], dest: &mut[u8], max_out_size: uint) -> int {
    use std::mem::size_of;

    let ctx = &mut [0u32, ..(1u << (MEMORY_USAGE - 2))];

    do ctx.as_mut_buf |ctx_p, _| {
        do source.as_imm_buf |src, src_size| {
            do dest.as_mut_buf |dst, _ | {
                unsafe {
                    if src_size < LIMIT64K {
                        compress_generic(ctx_p as *mut Lz4Data, src, dst, src_size, max_out_size,
                            Limited, ByU16, NoPrefix)
                    } else {
                        compress_generic(ctx_p as *mut Lz4Data, src, dst, src_size, max_out_size,
                            Limited, if size_of::<*()>() == 8 { ByU32 } else { ByPtr }, NoPrefix)
                    }
                }
            }
        }
    }
}

/// Decompresses `source` to `dest`, but may read outside of input buffer
/// # Arguments
/// * `source` - data to be decompressed
/// * `dest` - destination buffer
/// # Return values
/// Number of bytes read from `source` (i.e. the compressed size)  or negative value
/// on malformed input
/// # Safety note
/// May read outside of input buffer, so this must be used only on trusted input
pub fn decompress_fast(source: &[u8], dest: &mut[u8]) -> int {
    do source.as_imm_buf | src, _ | {
        do dest.as_mut_buf | dst, dst_len | {
            unsafe {
                decompress_generic(src, dst, 0, dst_len, OnOutPutSize, WithPrefix, Full, 0)
            }
        }
    }
}

/// Decompresses `source` to `dest`, safely, such that it never reads outside of input buffer
/// # Arguments
/// * `source` - data to be decompressed
/// * `dest` - destination buffer
/// * `target_out_size` - will attempt to stop decompression once this has been reached
/// # Return values
/// Returns the the number of bytes decoded in the `dest` buffer. (necessarily <= dest.len()) or
///  negative value on malformed input
pub fn decompress_safe_partial(source: &[u8], dest: &mut[u8], target_out_size: uint) -> int {
    do source.as_imm_buf | src, src_len | {
        do dest.as_mut_buf | dst, dst_len | {
            unsafe {
                decompress_generic(src, dst, src_len, dst_len,
                    OnInputSize, NoPrefix, Partial, target_out_size)
            }
        }
    }
}

//****************************
// Stream Compression Functions
//****************************



// pub fn compress_continue(ctx: &mut Lz4Data, source: &[u8], dest: &mut[u8]) -> int {
//     let ctx_p = ptr::to_mut_unsafe_ptr(ctx);
//
//     do source.as_imm_buf |src, src_size| {
//         do dest.as_mut_buf |dst, _ | {
//             unsafe {
//                 compress_generic(ctx_p, src, dst, src_size, 0, NotLimited, ByU32, WithPrefix)
//             }
//         }
//     }
// }

//**************************************
// Tuning parameters
//**************************************
// MEMORY_USAGE :
// Memory usage formula : N->2^N Bytes
//     (examples : 10 -> 1KB; 12 -> 4KB ; 16 -> 64KB; 20 -> 1MB; etc.)
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
pub static MAX_INPUT_SIZE: uint = 0x7E000000;

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

// macro_rules! expect(
//     ($expr:expr, $value:expr) => ( $expr )
// )
//
// macro_rules! likely(
//     ($expr:expr) => ( expect!($expr != false, true))
// )
//
// macro_rules! unlikely(
//     ($expr:expr) => ( expect!($expr != false, false))
// )

macro_rules! swap16(
    ($x:expr) => (::std::unstable::intrinsics::bswap16($x as i16))
)

// Structs and local types ========================
// ================================================

#[allow(missing_doc)]
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
        {
            (AARCH!($d)) = AARCH!($s);
            $d = $d.offset(STEPSIZE as int);
            $s = $s.offset(STEPSIZE as int);
        }
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
    ($p:expr, $i:expr) => (
        { let v = $i as u16;  v = bswap16!(v); (A16!($p)) = v; $p = $p.offset(2); }
    )
)

#[cfg(target_endian = "little")]
macro_rules! read_le16(
    ($d:expr, $s:expr, $p:expr) => ( $d = $s.offset(-(A16!($p) as int)); )
)

#[cfg(target_endian = "little")]
macro_rules! write_le16(
    ($p:expr, $v:expr) => ( { (A16!($p)) = $v; $p = $p.offset(2); } )
)


// Private functions ==============================
// ================================================
#[cfg(target_endian = "big", target_word_size = "64")]
fn nb_common_bytes(val: u64) -> i32 {
    unsafe { (intrinsics::ctlz64(val as i64) >> 3) as i32 }
}

#[cfg(target_endian = "little", target_word_size = "64")]
fn nb_common_bytes(val: u64) -> i32 {
    unsafe { (intrinsics::cttz64(val as i64) >> 3) as i32 }
}

#[cfg(target_endian = "big", target_word_size = "32")]
fn nb_common_bytes(val: u32) -> i32 {
    unsafe { (intrinsics::ctlz32(val as i32) >> 3) }
}

#[cfg(target_endian = "little", target_word_size = "32")]
fn nb_common_bytes(val: u32) -> i32 {
    unsafe { (intrinsics::cttz32(val as i32) >> 3) }
}

// Compression functions ==========================
// ================================================
#[inline(always)]
fn hash_sequence(sequence: u32, table_type: TableType) -> i32 {
    let ret = if table_type == ByU16 {
        ((sequence) *  2654435761u32) >> ((MINMATCH*8) - (HASHLOG+1))
    } else {
        ((sequence) * 2654435761u32) >> ((MINMATCH*8) - HASHLOG)
    } as i32;
    ret
}

#[inline(always)]
fn hash_position(p: *u8, table_type: TableType) -> i32 {
    hash_sequence(unsafe { A32!(p) }, table_type)
}

#[inline(always)]
fn put_position_on_hash(p: *u8, h: u32, table_base: *(), tt: TableType, src_base: *u8) {

    unsafe {
        match tt {
            ByPtr => {
                let hashtable = table_base as *mut *u8;
                *hashtable.offset(h as int) = p;
            }
            ByU32 => {
                let hashtable = table_base as *mut u32;
                *hashtable.offset(h as int) = p.offset(- (src_base as int)) as u32;
            }
            ByU16 => {
                let hashtable = table_base as *mut u16;
                *hashtable.offset(h as int) = p.offset(- (src_base as int)) as u16;
            }
        }
    }
}

#[inline(always)]
fn put_position(p: *u8, table_base: *(), tt: TableType, src_base: *u8) {
    let h: u32 = hash_position(p, tt) as u32;
    put_position_on_hash(p, h, table_base, tt, src_base);
}

#[inline(always)]
fn get_position_on_hash(h: u32, table_base: *(), tt: TableType, src_base: *u8) -> *u8 {
    unsafe {
        match tt {
            ByPtr => {
                let hashtable = table_base as **u8;
                *hashtable.offset(h as int)
            }
            ByU32 => {
                let hashtable = table_base as *u32;
                src_base.offset(*hashtable.offset(h as int) as int)
            }
            ByU16 =>  {
                let hashtable = table_base as *u16;
                src_base.offset(*hashtable.offset(h as int) as int)
            }
        }
    }
}

#[inline(always)]
fn get_position(p: *u8, table_base: *(), tt: TableType, src_base: *u8) -> *u8 {
    let h = hash_position(p, tt) as u32;
    get_position_on_hash(h, table_base, tt, src_base)
}

// #[inline(always)]
unsafe fn compress_generic(
    ctx: *mut Lz4Data,
    source: *u8,
    dest: *mut u8,
    input_size: uint,
    max_out_size: uint,
    lim_output: LimitiedOutput,
    tt: TableType,
    prefix: Prefix64k) -> int {

    let mut ip: *u8 = source;
    let base: *u8 = if prefix == WithPrefix {  (*ctx).base } else { source };
    let low_limit: *u8 = if prefix == WithPrefix {  (*ctx).buffer_start } else { source };
    let mut anchor: *u8 = source;
    let iend: *u8 = ip.offset(input_size as int);
    let mflimit: *u8 = iend.offset(-(MFLIMIT as int));
    let matchlimit: *u8 = iend.offset(-(LAST_LITERALS as int));

    let mut op: *mut u8 = dest;
    let oend: *mut u8 = op.offset(max_out_size as int);

    let mut length: int;
    let skip_strength = SKIP_STRENGTH as int;
    let mut forwardH: u32;

    // Init conditions
    if input_size > MAX_INPUT_SIZE {
        debug!("input_size > MAX_INPUT_SIZE");
        return 0;
    }
    if prefix == WithPrefix && ip != (*ctx).next_block {
        debug!("prefix == WithPrefix && ip != (*ctx).next_block");
        return 0;
    }
    if prefix == WithPrefix { (*ctx).next_block = iend; }
    if tt == ByU16 && input_size >= LIMIT64K {
        debug!("tt == ByU16 && input_size >= LIMIT64K");
        return 0;
    }
    if input_size < MINLENGTH {
        let mut lastrun = iend.offset(-(anchor as int)) as int;
        if lim_output == Limited && (op.offset(-(dest as int)))
            .offset(lastrun + 1 + ((lastrun + 255 - RUN_MASK as int)/255)) as uint
                > max_out_size {
            debug!("lim_output == Limited && (op.offset(-(dest as int)))
            .offset(lastrun + 1 + ((lastrun + 255 - RUN_MASK as int)/255)) as uint
                > max_out_size");
            return 0;
        }
        if lastrun >= RUN_MASK as int {
            *op = RUN_MASK as u8 << ML_BITS as u8; op = op.offset(1);
            lastrun -= RUN_MASK as int;
            while lastrun >= 255 {
                *op = 255; op = op.offset(1);
                *op = lastrun as u8; op = op.offset(1);
                lastrun -= 255
            }
        } else {
            *op = (lastrun << ML_BITS) as u8; op = op.offset(1);
        }
        ::std::ptr::copy_memory(op, anchor, iend.offset(-(anchor as int))  as uint);
        op = op.offset(iend.offset(-(anchor as int))  as int);

        return op.offset(-(dest as int)) as int;
    }

    // First byte
    put_position(ip, ctx as *(), tt, base);
    ip = ip.offset(1);
    forwardH = hash_position(ip, tt) as u32;

    // Labeled loops are being used to emulate GOTOs found in the C code here:
    // https://code.google.com/p/lz4/source/browse/trunk/lz4.c#406
    // Main loop
    'outer : loop {
        let mut find_match_attempts: int = (1 << skip_strength) + 3;
        let mut forward_ip: *u8 = ip;
        let mut reff: *u8;
        let mut token: *mut u8;

        // Find a match
        do_while!({
            let h: u32 = forwardH;
            let step: int = find_match_attempts >> skip_strength; find_match_attempts += 1;
            ip = forward_ip;
            forward_ip = ip.offset(step);

            if forward_ip > mflimit {
                // goto _last_literals
                break 'outer;
            }

            forwardH = hash_position(forward_ip, tt) as u32;
            reff = get_position_on_hash(h, ctx as *(), tt, base);
            put_position_on_hash(ip, h, ctx as *(), tt, base);
        }, reff.offset(MAX_DISTANCE as int) < ip || A32!(reff) != A32!(ip)  )

        // Catch up
        while ip > anchor && reff > low_limit && *ip.offset(-1) == *reff.offset(-1) {
            ip = ip.offset(-1);
            reff = reff.offset(-1);
        }

        // Encode literal length
        length = ip.offset(-(anchor as int)) as int;
        token = op; op = op.offset(1);
        if lim_output == Limited &&
                op.offset(length + (2 + 1 + LAST_LITERALS) as int + (length >> 8)) > oend {
            debug!("lim_output == Limited &&
            op.offset(length + 2 + 1 + LAST_LITERALS as int + length >> 8) > oend");
            return 0;
        }
        if length >= RUN_MASK as int {
            let mut len: int = length - RUN_MASK as int;
            *token = (RUN_MASK << ML_BITS) as u8;
            while len >= 255 {
                *op = 255; op = op.offset(1);
                len -= 255;
            }
            *op = len as u8; op = op.offset(1);
        } else {
            *token = (length << ML_BITS) as u8;
        }

        // Copy literals
        {
            let end: *mut u8 = op.offset(length);
            wild_copy!(op, anchor, end);
            op = end;
        }

        'next_match : loop {
            // Write offset
            write_le16!(op, ip.offset(-(reff as int)) as u16);

            // Start counting
            ip = ip.offset(MINMATCH as int); reff = reff.offset(MINMATCH as int);
            anchor = ip;

            // used to emulate goto here:
            // https://code.google.com/p/lz4/source/browse/trunk/lz4.c#497
            let mut skip = false;

            while ip < matchlimit.offset(-((STEPSIZE - 1) as int)) {
                let diff: uint = AARCH!(reff) ^ AARCH!(ip);
                if diff == 0 {
                    ip = ip.offset(STEPSIZE as int); reff = reff.offset(STEPSIZE as int);
                    continue;
                }
                ip = ip.offset(nb_common_bytes(diff as u64) as int);
                // used to emulate goto here:
                // https://code.google.com/p/lz4/source/browse/trunk/lz4.c#497
                skip = true; break;
            }

            if !skip {
                if ARCH64 {
                    if ip < matchlimit.offset(-3) && A32!(reff) == A32!(ip) {
                        ip = ip.offset(4); reff = reff.offset(4);
                    }
                }
                if ip < matchlimit.offset(-1) && A16!(reff) == A16!(ip) {
                    ip = ip.offset(2); reff = reff.offset(2);
                }
                if ip < matchlimit && *reff == *ip {
                    ip = ip.offset(1);
                }
            }

            'end_count : loop {
                // Encode match length
                length = ip.offset(-(anchor as int)) as int;
                if lim_output == Limited && (op.offset(1i + LAST_LITERALS as int + (length >> 8))
                    > oend) {
                    debug!("lim_output == Limited && (op.offset(1i + LAST_LITERALS as
                    int + length >> 8) > oend)");
                    return 0;
                }
                if length >= ML_MASK as int {
                    *token += ML_MASK as u8;
                    length -= ML_MASK as int;
                    while length > 509 {
                        *op = 255; op = op.offset(1);
                        *op = 255; op = op.offset(1);
                        length -= 510;
                    }
                    if length >= 255 {
                        length -= 255;
                        *op = 255; op = op.offset(1);
                    }
                    *op = length as u8; op = op.offset(1);
                } else {
                    *token += length as u8;
                }

                // Test end of chunk
                if ip > mflimit {
                    anchor = ip;
                    break 'outer;
                }

                // Fill table
                put_position(ip.offset(-2), ctx as *(), tt, base);

                // Test next position
                reff = get_position(ip, ctx as *(), tt, base);
                put_position(ip, ctx as *(), tt, base);
                if reff.offset(MAX_DISTANCE as int) >= ip && A32!(reff) == A32!(ip) {
                    token = op; op = op.offset(1);
                    *token = 0;
                    continue 'next_match;
                }

                // Prepare next loop
                anchor = ip; ip = ip.offset(1);
                forwardH = hash_position(ip, tt) as u32;

                continue 'outer;
            }
        }
    }

    // Emulating _last_literals label
    {
        let mut lastrun = iend.offset(-(anchor as int)) as int;
        if lim_output == Limited && (op.offset(-(dest as int)))
            .offset(lastrun + 1 + ((lastrun + 255 - RUN_MASK as int)/255)) as uint >
            max_out_size {
            return 0;
        }
        if lastrun >= RUN_MASK as int {
            *op = RUN_MASK as u8 << ML_BITS as u8; op = op.offset(1);
            lastrun -= RUN_MASK as int;
            while lastrun >= 255 {
                *op = 255; op = op.offset(1);
                *op = lastrun as u8; op = op.offset(1);
                lastrun -= 255
            }
        } else {
            *op = (lastrun << ML_BITS) as u8; op = op.offset(1);
        }
        ptr::copy_memory(op, anchor, iend.offset(-(anchor as int))  as uint);
        op = op.offset(iend.offset(-(anchor as int))  as int);
    }

    op.offset(-(dest as int)) as int
}

// #[inline(always)]
unsafe fn decompress_generic(source: *u8,
    dest: *mut u8,
    input_size: uint,
    output_size: uint,
    end_on_input: EndCondition,
    prefix64k: Prefix64k,
    partial_decoding: EarlyEnd,
    target_out_size: uint) -> int {
    use std::uint;
    use std::mem::size_of;

    // Local variables
    let mut ip: *u8 = source;
    let mut reff: *u8;
    let iend: *u8 = ip.offset(input_size as int);

    let mut op: *mut u8 = dest;
    let oend: *mut u8 = op.offset(output_size as int);
    let mut cpy: *mut u8;
    let mut oexit: *mut u8 = op.offset(target_out_size as int);

    let dec32table: &[uint] = &[0, 3, 2, 3, 0, 0, 0, 0];
    let dec64table: &[uint] = &[0, 0, 0, uint::max_value, 0, 1, 2, 3];

    // Special cases
    if partial_decoding == Partial && oexit > oend.offset(-(MFLIMIT as int)) as *mut u8 {
        oexit = oend.offset(-(MFLIMIT as int)) as *mut u8;
    }
    if end_on_input == OnInputSize && output_size == 0 {
        return if input_size == 1 && *ip == 0 { 0 } else { -1 };
    }
    if end_on_input == OnOutPutSize && output_size == 0 {
        return if *ip == 0 { 1 } else { -1 };
    }

    // emulating _output_error label and gotos
    'decode: loop {
        // Main loop
        loop {
            let mut token: uint;
            let mut length: uint;

            // Get runlength
            token = *ip as uint; ip = ip.offset(1);
            length = token >> ML_BITS;

            if length == RUN_MASK {
                let mut s: uint = 255;

                while if end_on_input == OnInputSize { ip < iend } else { true } && s == 255 {
                    s = *ip as uint; ip = ip.offset(1);
                    length += s;
                }
            }
// Hellish C conditional for reference
//
// (((endOnInput) && ((cpy>(partialDecoding?oexit:oend-MFLIMIT)) || (ip+length>iend-(2+1+LASTLITERALS))) )
//             || ((!endOnInput) && (cpy>oend-COPYLENGTH)))

            // Copy literals
            cpy = op.offset(length as int);
            if (((end_on_input == OnInputSize) &&
                ((cpy > (if partial_decoding == Partial { oexit }
                    else { oend.offset(-(MFLIMIT as int)) as *mut u8 })) ||
                (ip.offset(length as int) > iend.offset(-(2 + 1 + LAST_LITERALS as int)))) ) ||
                ((end_on_input == OnOutPutSize) &&
                    (cpy > oend.offset(-(COPY_LENGTH as int)) as *mut u8))) {

                if partial_decoding == Partial {
                    if cpy > oend { break 'decode; }
                    if end_on_input == OnInputSize &&
                        ip.offset(length as int) > iend { break 'decode; }
                } else {
                    if end_on_input == OnOutPutSize && cpy != oend { break 'decode; }
                    if end_on_input == OnInputSize &&
                        (ip.offset(length as int) != iend || cpy > oend) { break 'decode; }
                }

                ptr::copy_memory(op, ip, length as uint);
                ip = ip.offset(length as int);
                op = op.offset(length as int);
                break;
            }
            wild_copy!(op, ip, cpy);

            ip = ip.offset(-(op.offset(-(cpy as int)) as int));
            op = cpy;

            // Get offset
            read_le16!(reff, cpy as *u8, ip); ip = ip.offset(2);
            if prefix64k == NoPrefix && reff < dest as *u8 { break 'decode; }

            // Get match length
            length = token as uint & ML_MASK;
            if length == ML_MASK {
                // Ensure enough bytes remain for LASTLITERALS + token
                while end_on_input == OnOutPutSize ||
                    ip < iend.offset(-(LAST_LITERALS as int + 1)) {
                    let s: uint = *ip as uint; ip = ip.offset(1);
                    length += s;
                    if s == 255 { continue; }
                    break;
                }
            }

            // Copy repeated sequence
            if (op.offset(-(reff as int)) as uint) < STEPSIZE {
                let dec64: uint = dec64table[if size_of::<*()>() == 4 { 0 }
                                                else { op.offset(-(reff as int)) as uint } ];
                *op = *reff;
                *op.offset(1) = *reff.offset(1);
                *op.offset(2) = *reff.offset(2);
                *op.offset(3) = *reff.offset(3);
                op = op.offset(4); reff = reff.offset(4);
                reff = reff.offset(-(dec32table[op.offset(-(reff as int)) as int] as int));
                (A32!(op)) = A32!(reff);
                op = op.offset(STEPSIZE as int - 4);
                reff = reff.offset(-(dec64 as int));
            } else {
                copy_step!(op, reff);
            }

            cpy = op.offset(length as int - (STEPSIZE as int -4));

            if cpy > oend.offset((-COPY_LENGTH as int - (STEPSIZE as int - 4i))) {
                // Error : last 5 bytes must be literals
                if cpy > oend.offset(-(LAST_LITERALS as int)) { break 'decode; }
                secure_copy!(op, reff, oend.offset(-(COPY_LENGTH as int)));
                while op < cpy {
                    *op = *reff;
                    op = op.offset(1);
                    reff = reff.offset(1);
                }
                op = cpy;
                continue;
            }

            wild_copy!(op, reff, cpy);
            op = cpy;
        }

        if end_on_input == OnInputSize {
            debug!("return op.offset(-(dest as int)) as int;");
            return op.offset(-(dest as int)) as int; // # of output bytes decoded
        } else {
            debug!("return ip.offset(-(source as int)) as int;");
            return ip.offset(-(source as int)) as int; // # of input bytes read
        }
    }

    // emulating _output_error label and gotos
    debug!("Output error: -(ip.offset(-(source as int)) as int) - 1;");
    return -(ip.offset(-(source as int)) as int) - 1;
}

#[cfg(test)]
mod test {
    use super::{compress, decompress_safe, compress_bound};
    use std::rand;
    use std::rand::Rng;
    use std::vec;
    use std::io::File;

    #[test]
    fn test_compression_correct() {
        let orig_path = Path::new("LICENSE.txt");
        let comp_path = Path::new("LICENSE.txt.lz4");

        let mut orig_f = File::open(&orig_path);
        let mut comp_f = File::open(&comp_path);

        let orig_data = orig_f.read_to_end();
        let known_compressed = comp_f.read_to_end();

        let mut out_buf = vec::from_elem(compress_bound(orig_data.len()).unwrap(),
            0u8);

        let comp_len = compress(orig_data, out_buf);
        let test_compressed = out_buf.slice_to(comp_len as uint);

        println!("Original length: {}, Known Compression Length: {},
            Test Compression Length: {}", orig_data.len(), known_compressed.len(),
            test_compressed.len());

        assert!(test_compressed == known_compressed);
    }

    #[test]
    fn test_decompression_correct() {
        let orig_path = Path::new("LICENSE.txt");
        let comp_path = Path::new("LICENSE.txt.lz4");

        let mut orig_f = File::open(&orig_path);
        let mut comp_f = File::open(&comp_path);

        let orig_data = orig_f.read_to_end();
        let known_compressed = comp_f.read_to_end();

        let mut out_buf = vec::from_elem(compress_bound(orig_data.len()).unwrap(),
            0u8);

        let decomp_len = decompress_safe(known_compressed, out_buf);
        let test_decompressed = out_buf.slice_to(decomp_len as uint);

        println!("Original length: {}, Test Decompressed Length: {}",
            orig_data.len(), test_decompressed.len());

        assert!(test_decompressed == orig_data);
    }

//     #[test]
    fn test_reversible() {
        let mut rng = rand::rng();
        let original_len = 1024 * 1024;
        let max_out = compress_bound(original_len);

        let original_data = rng.gen_vec(original_len);
        let mut compress_buf = vec::from_elem(max_out.unwrap(), 0u8);
        let mut decompress_buf = vec::from_elem(max_out.unwrap(), 0u8);

        let compressed_size = compress(original_data, compress_buf);
        println!("MAXOUT: {}, Original size: {}, Compressed size: {}, Compressed/Original ratio {}%",
            max_out, original_len, compressed_size,
            100f32 * compressed_size as f32 / original_len as f32);

        let com_slice = compress_buf.slice_to(compressed_size as uint);

        let decompressed_size = decompress_safe(com_slice, decompress_buf);
        println!("AAAAAAAAAAA {}", decompressed_size);
        let dec_slice = decompress_buf.slice_to(decompressed_size as uint);
        println!("AAAAAAAAAAA");
        assert!(decompressed_size as uint == original_len);
        assert!(original_data.as_slice() == dec_slice);
    }

}