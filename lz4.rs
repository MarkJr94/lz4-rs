


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
static HEAPMODE: uint = 0;

if_x64!()
pub static ARCH64: bool = true;

type byte = u8;