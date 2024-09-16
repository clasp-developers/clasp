#pragma once

#define TAGGED_POINTER 1

/// USE_BOEHM_MEMORY_MARKER may be useful for debugging processes that cons memory
/// I lets you write an integer (by using (gctools:gc-marker <int>) into the header
/// of every object allocated while that marker is set.
/// Then you can use (room t <int>) to dump only those objects marked with the marker

// #define USE_BOEHM_MEMORY_MARKER

/// Tracking allocations with TRACK_ALLOCATIONS keeps a count of
/// exactly how many bytes are CONSed by Clasp
/// Compiling min-boehm-recompile with it defined 4:54 min and off 4.56 min
/// so it has no significant impact at this stage
#define TRACK_ALLOCATIONS // this may slow down allocation

///
/// Only define one of MPS_RECOGNIZE_ALL_TAGS or MPS_RECOGNIZE_ZERO_TAG or neither
/// MPS_RECOGNIZE_ALL_TAGS allows any value in the lower three bits to be considered as a pointer
/// MPS_RECOGNIZE_ZERO_TAG allows ( ZERO_TAG_MASK | ptr ) == 0 to be considered as a pointer
// #define MPS_RECOGNIZE_ALL_TAGS   // Anything can be a pointer - overrides MPS_RECOGNIZE_ZERO_TAG
#define MPS_RECOGNIZE_ZERO_TAG // recognize #b000 as a tagged pointer
#define TAG_BITS 3

// For MPS extensions can define custom allocation points - but only up to this many
#define MAX_CUSTOM_ALLOCATION_POINTS 4

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define ENDIAN_LSB_OFFSET 0
#else
#define ENDIAN_LSB_OFFSET 7
#endif
// Match tags using (ptr&MATCH_TAG_MASK)==MATCH_TAG_EQ
// These values are used in point
//
// When you add/remove tags you will need to change write_ugly_object so that it can print that
// tagged object and you will need to change _rep_ in foundation.cc

#define CONS_HEADER_SIZE 0 // CONS has no header
#define FIXNUM_MASK 0x03
#define FIXNUM0_TAG 0x00
#define FIXNUM1_TAG 0x04 // fixnum means lower two bits are zero so two tags
#define FIXNUM_SHIFT 2
#define GENERAL_TAG 0b001
#define CHARACTER_TAG 0b010
#define CHARACTER_SHIFT TAG_BITS
#define CONS_TAG 0x03
#define VASLIST0_TAG 0x05
#define UNBOUND_TAG 0x07
#ifdef CLASP_SHORT_FLOAT
#define SHORT_FLOAT_TAG 0b1100
#define SINGLE_FLOAT_TAG 0b1101
#define SHORT_FLOAT_SHIFT (TAG_BITS + 1)
#define SINGLE_FLOAT_SHIFT (TAG_BITS + 1)
#else
#define SINGLE_FLOAT_TAG 0b110
#define SINGLE_FLOAT_SHIFT TAG_BITS
#endif
#if TAG_BITS == 3
#define CLASP_ALIGNMENT 8
#define ZERO_TAG_MASK 0x07
#define GC_TAG 0x07
#define IMMEDIATE_MASK 0x07
#define VASLIST_ALIGNMENT 8
// The following are different unbound-like values that can be tested for by
// comparing the least significant byte of a pointer to the following bytes.
#define UNBOUND_MASK ((uintptr_t)0xFF)
#define UNBOUND_BYTE ((uint8_t)(0x0 | UNBOUND_TAG))
#define NO_THREAD_LOCAL_BINDING_UNBOUND_BYTE ((uint8_t)(0x40 | UNBOUND_TAG))
#define NO_KEY_UNBOUND_BYTE ((uint8_t)(0x10 | UNBOUND_TAG))
#define DELETED_UNBOUND_BYTE ((uint8_t)(0x18 | UNBOUND_TAG))
#define SAME_AS_KEY_UNBOUND_BYTE ((uint8_t)(0x20 | UNBOUND_TAG))
#else // TAG_BITS==4
#define CLASP_ALIGNMENT 16
#define ZERO_TAG_MASK 0x0F
#define GC_TAG 0x0F
#define IMMEDIATE_MASK 0x0F
#define FIXNUM2_TAG 0x08 // fixnum means lower two bits are zero so four tags (if TAG_BITS==4)
#define FIXNUM3_TAG 0x0C // fixnum means lower two bits are zero so four tags (if TAG_BITS==4)
#define VASLIST_ALIGNMENT 16
#endif

/*! A test for pointers that MPS needs to fix/manage has the form (potential_ptr&POINTER_TAG_MASK)==POINTER_TAG_EQ)
    MPS needs to manage tagged pointers with POINTER_GENERAL_TAG or POINTER_CONS_TAG and nothing else.
    POINTER_GENERAL_TAG and POINTER_CONS_TAG objects are the only objects that are moved/fixed/updated by MPS.
    This will recognize 0x03 (CONS_TAG) and 0x01 (GENERAL_TAG) and not anything else ie: 0x05 (VALIST_S)*/
#define POINTER_TAG_MASK ((~(GENERAL_TAG ^ CONS_TAG)) & ZERO_TAG_MASK)
#define POINTER_TAG_EQ (GENERAL_TAG & CONS_TAG)

///------------------------------------------------------------
/// USE_STATIC_ANALYZER_GLOBAL_SYMBOLS
///
/// If USE_SYMBOLS_IN_GLOBAL_ARRAY is undefined then
/// symbols are fixed using either those extracted using the static analyzer
/// or by the scraper.

// #define USE_STATIC_ANALYZER_GLOBAL_SYMBOLS

/// USE_SYMBOLS_IN_GLOBAL_ARRAY
/// Puts all global symbols in one large array
/// and they are fixed in gc_interface.cc with a loop

#define USE_SYMBOLS_IN_GLOBAL_ARRAY

/// The size of the sigaltstack that Clasp requires to do at least some Common Lisp calls
/// 1 MB is large - if we have a lot of threads we will want to knock this down
#define SIGNAL_STACK_SIZE (1024 * 1024)

#define CLASP_DESIRED_STACK_CUR 16 * 1024 * 1024

/// ----------------------------------------------------------------------
///
/// MPS debugging options
///

// #define DEBUG_THROW_IF_INVALID_CLIENT_ON
