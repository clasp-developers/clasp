#ifndef configure_memory_H
#define configure_memory_H

#define TAGGED_POINTER 1



/// USE_BOEHM_MEMORY_MARKER may be useful for debugging processes that cons memory
/// I lets you write an integer (by using (gctools:gc-marker <int>) into the header
/// of every object allocated while that marker is set.
/// Then you can use (room t <int>) to dump only those objects marked with the marker

//#define USE_BOEHM_MEMORY_MARKER

#ifdef USE_BOEHM
 #ifdef USE_CXX_DYNAMIC_CAST
  #define BIG_BOEHM_HEADER
 #endif
#endif

/// Define USE_ALLOCA_FOR_FRAME to use alloca to create Lisp stack frames within the C++ stack
/// otherwise use the garbage collector to maintain a separate stack for lisp frames
///
#ifdef USE_BOEHM
#define USE_ALLOCA_FOR_FRAME
#endif

#ifdef USE_MPS
#define USE_ALLOCA_FOR_FRAME
#endif

/// Tracking allocations with TRACK_ALLOCATIONS keeps a count of
/// exactly how many bytes are CONSed by Clasp
/// Compiling min-boehm-recompile with it defined 4:54 min and off 4.56 min
/// so it has no significant impact at this stage
#define TRACK_ALLOCATIONS // this may slow down allocation

///
/// Only define one of MPS_RECOGNIZE_ALL_TAGS or MPS_RECOGNIZE_ZERO_TAG or neither
/// MPS_RECOGNIZE_ALL_TAGS allows any value in the lower three bits to be considered as a pointer
/// MPS_RECOGNIZE_ZERO_TAG allows ( ZERO_TAG_MASK | ptr ) == 0 to be considered as a pointer
//#define MPS_RECOGNIZE_ALL_TAGS   // Anything can be a pointer - overrides MPS_RECOGNIZE_ZERO_TAG
#define MPS_RECOGNIZE_ZERO_TAG   // recognize #b000 as a tagged pointer
#define ZERO_TAG_MASK       0x07          // goes with MPS_RECOGNIZE_ZERO_TAG

// Match tags using (ptr&MATCH_TAG_MASK)==MATCH_TAG_EQ
// These values are used in point
#define FIXNUM_MASK         0x03
#define FIXNUM0_TAG         0x00
#define CPTR_TAG            0x01
#define CHARACTER_TAG       0x02
#define CONS_TAG            0x03
#define FIXNUM1_TAG         0x04
#define VASLIST_TAG         0x05
#define SINGLE_FLOAT_TAG    0x06
#define GENERAL_TAG         0x07
#define IMMEDIATE_MASK      0x07
#define SINGLE_FLOAT_SHIFT  3
#define CHARACTER_SHIFT     3



  /*! A test for pointers that MPS needs to fix/manage has the form (potential_ptr&POINTER_TAG_MASK)==POINTER_TAG_EQ) 
      MPS needs to manage tagged pointers with POINTER_GENERAL_TAG or POINTER_CONS_TAG and nothing else.
      POINTER_GENERAL_TAG and POINTER_CONS_TAG objects are the only objects that are moved/fixed/updated by MPS.
      This will recognize 0x03 and 0x07 and not anything else ie: 0x05 (VALIST_S)*/
#define POINTER_TAG_MASK    0x03  
#define POINTER_TAG_EQ      0x03

  
  /* Discriminator functions need to know where to find the stamp.
   It can either be in the header, the rack (instance, funcallable-instance)
   the wrapper (WrappedPointer_O) or call the get_stamp() virtual function
   that all General_O objects provide.
   The following values define the four possibilities.
*/
  
#define FLAGS_STAMP_IN_HEADER     0x0
#define FLAGS_STAMP_IN_RACK       0x1
#define FLAGS_STAMP_IN_WRAPPER    0x2
#define FLAGS_STAMP_IN_CALLBACK   0x3

#define FLAGS_INSTANCE FLAGS_STAMP_IN_RACK
#define FLAGS_FUNCALLABLE_INSTANCE FLAGS_STAMP_IN_RACK

///------------------------------------------------------------
/// USE_STATIC_ANALYZER_GLOBAL_SYMBOLS
///
/// If USE_SYMBOLS_IN_GLOBAL_ARRAY is undefined then
/// symbols are fixed using either those extracted using the static analyzer
/// or by the scraper.

//#define USE_STATIC_ANALYZER_GLOBAL_SYMBOLS


/// USE_SYMBOLS_IN_GLOBAL_ARRAY
/// Puts all global symbols in one large array
/// and they are fixed in gc_interface.cc with a loop

#define USE_SYMBOLS_IN_GLOBAL_ARRAY


/// ----------------------------------------------------------------------
///
/// MPS debugging options
///

//#define DEBUG_THROW_IF_INVALID_CLIENT_ON
#endif
