#pragma once

// #ifndef _clasp_memoryManagement_H
// #define _clasp_memoryManagement_H

/* Roots
 *
 * The following are garbage collector roots
 *
 * DEFINED in gctools/memoryManagement.h
 *   Lisp* ::_lisp
 *  This is a tagged pointer to a Lisp object
 *
 * DEFINED in include/clasp/gctools/globals.h
 *    extern core::Symbol_O* gctools::global_core_symbols[NUMBER_OF_CORE_SYMBOLS]
 *   These are tagged pointers to Symbol_O objects
 *
 * DEFINED in include/clasp/gctools/exposeFunctions.h
 *   extern gctools::smart_ptr<core::Symbol_O>  gctools::global_symbols[global_symbol_count];
 */

// Define compile-time flags that effect structure sizes
//
#include <atomic>
#include <clasp/gctools/configure_memory.h>
#include <clasp/gctools/hardErrors.h>

#if defined(USE_BOEHM)
#ifdef CLASP_THREADS
#define GC_THREADS
#endif
#include "src/bdwgc/include/gc.h"
#include "src/bdwgc/include/gc_mark.h"
extern "C" {
#include "src/bdwgc/include/gc_inline.h"
};
#elif defined(USE_MMTK)
#include <mmtk/api/mmtk.h>
#elif defined(USE_MPS)
extern "C" {
#include <clasp/mps/code/mps.h>
#include <clasp/mps/code/mpsavm.h>
};
#endif

namespace core {
struct ThreadLocalState;
struct LispHolder;
} // namespace core
namespace gctools {

struct ClaspInfo {
  int _argc;
  const char** _argv;
  size_t _stackMax;
  core::LispHolder* _lispHolder;
  bool _mpiEnabled;
  int _mpiRank;
  int _mpiSize;

  ClaspInfo(int argc, const char** argv, size_t stackMax)
      : _argc(argc), _argv(argv), _stackMax(stackMax), _lispHolder(NULL), _mpiEnabled(false), _mpiRank(0), _mpiSize(1){};
};
}; // namespace gctools

#define GC_LOG(x)
#define GCPRIVATE public
#define GCPROTECTED public

#include <clasp/gctools/hardErrors.h>

#ifdef RUNNING_PRECISEPREP
typedef uintptr_t GC_word;
inline void* GC_base(void* v) { return v; };
#endif

namespace gctools {

typedef enum { room_test, room_max, room_default, room_min } RoomVerbosity;

// The clasp pointer type.
// This was introduced very late - but we can use it to recognize pointers that need fixup in save/load
typedef unsigned char* clasp_ptr_t;

}; // namespace gctools

// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
//
// Define what a Header_s is for each garbage collector
// as well as other GC specific stuff
//
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------

namespace gctools {
/*! Specialize GcKindSelector so that it returns the appropriate GcKindEnum for OT */
template <class OT> struct GCStamp;
extern size_t global_alignup_sizeof_header;
extern const char* _global_stack_marker;
extern size_t _global_stack_max_size;
}; // namespace gctools

// #define FWD_MTAG            0b010
// #define MASK_MTAG           0b111
#define DO_SHIFT_STAMP(unshifted_stamp)                                                                                            \
  ((unshifted_stamp << gctools::Header_s::general_mtag_shift) | gctools::Header_s::general_mtag)

#define STAMP_UNSHIFT_WTAG(stampwtag) (((size_t)stampwtag) >> gctools::Header_s::wtag_width)
#define STAMP_UNSHIFT_MTAG(unshifted_stamp) (((size_t)unshifted_stamp) >> gctools::Header_s::general_mtag_shift)
// ADJUST_STAMP values are left unshifted
#define ADJUST_STAMP(unshifted_stamp) (unshifted_stamp) // (unshifted_stamp<<STAMP_PARTIAL_SHIFT_REST_FIXNUM)|STAMP_MTAG)
// ISA_ADJUST_STAMP must be shifted so that they match header values when they are read
//     straight out of a header (they will already be shifted)
#define ISA_ADJUST_STAMP(unshifted_stamp) DO_SHIFT_STAMP(unshifted_stamp)
// TYPEQ_ADJUST_STAMP will be passed to make_fixnum - so it will be shifted
#define TYPEQ_ADJUST_STAMP(unshifted_stamp) (unshifted_stamp)

namespace gctools {

class Header_s;

template <typename T> struct GCHeader {
  typedef Header_s HeaderType;
};

template <typename T> struct GCAllocationPoint;
}; // namespace gctools

/*!
  Template struct:   DynamicCast

*/
#include <clasp/gctools/gc_boot.h>
#include <clasp/gctools/pointer_tagging.h>

namespace gctools {
/*! This is the type of the tagged kind header that is the first
word of every object in memory managed by the GC */
typedef uint32_t stamp_t;
typedef uint32_t tagged_stamp_t;

}; // namespace gctools

namespace gctools {
class GCObject {};
}; // namespace gctools

#include <clasp/gctools/threadlocal.fwd.h>

extern "C" {
const char* obj_name(gctools::stamp_t kind);
extern void obj_dump_base(void* base);
};

namespace gctools {
extern bool global_debuggerOnSIGABRT; // If this is false then SIGABRT is processed normally and it will lead to termination of the
                                      // program. See core__exit!
};                                    // namespace gctools

namespace gctools {
/*! Allocate an atomic buffer with malloc */
char* clasp_alloc_atomic(size_t buffer_size);
/*! The buffer above must be deallocated using this call*/
void clasp_dealloc(char* buffer);
}; // namespace gctools

namespace gctools {

#define STAMPWTAG_DUMMY_FOR_CPOINTER 0
typedef enum {
#if !defined(SCRAPING)
#if !defined(USE_PRECISE_GC)
#define GC_ENUM
#include INIT_CLASSES_INC_H // REPLACED CLASP_GC_CC
#undef GC_ENUM
#else
#define GC_ENUM
#include CLASP_GC_CC
#undef GC_ENUM
#endif
#endif
  STAMPWTAG_VASLIST_S = STAMPWTAG_core__Vaslist_dummy_O,
  STAMPWTAG_CONS = STAMPWTAG_core__Cons_O,
  STAMPWTAG_CHARACTER = STAMPWTAG_core__Character_dummy_O,
  STAMPWTAG_UNUSED = STAMPWTAG_core__Unused_dummy_O,
  STAMPWTAG_CPOINTER = STAMPWTAG_DUMMY_FOR_CPOINTER,
  STAMPWTAG_SINGLE_FLOAT = STAMPWTAG_core__SingleFloat_dummy_O,
  STAMPWTAG_FIXNUM = STAMPWTAG_core__Fixnum_dummy_O,
  STAMPWTAG_INSTANCE = STAMPWTAG_core__Instance_O,
  STAMPWTAG_FUNCALLABLE_INSTANCE = STAMPWTAG_core__FuncallableInstance_O,
  STAMPWTAG_WRAPPED_POINTER = STAMPWTAG_core__WrappedPointer_O,
  STAMPWTAG_DERIVABLE = STAMPWTAG_core__DerivableCxxObject_O,
  STAMPWTAG_CLASS_REP = STAMPWTAG_clbind__ClassRep_O
} GCStampEnum;

// These different positions represent tag tests in the dtree interpreter and
//   discriminating functions
#define FIXNUM_TEST 0x00
#define SINGLE_FLOAT_TEST 0x01
#define CHARACTER_TEST 0x02
#define CONS_TEST 0x03

}; // namespace gctools

namespace gctools {
constexpr size_t Alignment() {
  static_assert(CLASP_ALIGNMENT == 8);
  return CLASP_ALIGNMENT;
};
inline constexpr size_t AlignUp(size_t size) { return (size + Alignment() - 1) & ~(Alignment() - 1); };
inline constexpr uintptr_t AlignUp(uintptr_t size, size_t alignment) { return (size + alignment - 1) & ~(alignment - 1); };
inline constexpr uintptr_t AlignDown(uintptr_t size, size_t alignment) { return size & ~(alignment - 1); };

}; // namespace gctools

namespace gctools {
class ThreadLocalState;
};

/*! Declare this in the top namespace */
extern THREAD_LOCAL core::ThreadLocalState* my_thread;

namespace gctools {

#define NO_BADGE 1
#define ILLEGAL_BADGE 0

/*! Stamp - integer value that is written into the header in normal general objects
            and into the Rack for Instance_O objects.
  See Header_s below for a description of the GC Header tag scheme.
  Stamp needs to fit within a Fixnum.
*/
typedef uintptr_t UnshiftedStamp; // first 62 bits
typedef uintptr_t ShiftedStamp;   // High 62 bits
extern std::atomic<UnshiftedStamp> global_NextUnshiftedStamp;

template <class T> inline size_t sizeof_with_header();

// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
//
// Define Header and stuff that can exist in the header
//
//

/*!
      A header is 8 bytes long and consists of one uintptr_t (8 bytes) value.
      The structure of the header is...

      The (header) uintptr_t is a tagged value where the
      two least significant bits are the tag.

                        badge  | stamp-value| where-tag |   mtag
      64 bits total -> 32 bits |    27 bits |   2 bits  | 3 bits

      The 'mtag' - three least-significant bits of the header uintptr_t value describe
      what the rest of the header data means.  This is used for General_O and derived objects.
      #B000 == 'stamp-tag' THIS MUST ALWAYS BE #B000 !!!!!!! EXCEPT MPS WILL CHANGE THEM FOR ITS PURPOSES.
              To be a valid Clasp object they must be #B000  The header value MUST look like a FIXNUM
              This is the 'stamp_tag' and indicates that the other bits in the header
              represent a stamp value that indicate whether there is an extended-stamp
              and where to find the extended-stamp.
      #B001 == invalid0_mtag This is an illegal value for the two lsbs,
              it indictates that this is not a valid header.
              This pattern is used to indicate a CONS header
      #B010 == (MPS specific) weak_mtag - A weak object
      #B011 == cons_mtag  This indicates that what follows is a cons cell.
      #B100 == invalid1_mtag
      #B101 == fwd_mtag - This tag indicates that the remaining data bits in the header contains a forwarding
              pointer.  The uintptr_t in additional_data[0] contains the length of
              the block from the client pointer.
      #B110   contains a pad; check the
              bit at #B100 to see if the pad is a pad1 (==0) or a pad (==1)
      #B111 == (MPS specific) This indicates that the header contains a pad; check the
              bit at #B100 to see if the pad is a pad1 (==0) or a pad (==1)

      IMPORTANT!!!!!:
          The header values are designed to look like FIXNUMs - so we can read them
          out of the header and compare them to stamps without shifting or masking anything.
          This is confusing because if you read the header value #B10100 in Clasp
          and print it you will see 5 (five) but you expect 20 (twenty).
          This is because the value is treated as a FIXNUM within Clasp and
          it looks like it's shifted >>2


      If the mtag is a 'stamp_tag' then the data bits have this meaning...
                              stamp   where_tag
      64 bits total -> |    27 bits | 2 bits |

      The 'stamp' is a 60 bit value (zero extended) that tells
      the MPS GC and the TYPEQ machinery what the layout of the object is and is used to determine
      IsA relationships between classes.

      The 'where_tag' (see derivable_wtag,rack_wtag,wrapped_wtag,header_wtag)
      tells clasp where to find the stamp value that is useful for fastgf dispatch.
      header_wtag says its in the header
      rack_wtag says it's in the rack
      wrapped_wtag says its in the wrapped pointer object
      derivable_wtag is complicated.  A C++ virtual method is called that returns the stamp.

      The STAMP is also used by the fastgf generic function dispatch method.
      Exposed C++ classes that inherit from Instance_O store an extended-stamp in the rack.
      The header stamp is used to tell the system when it needs to look elsewhere for the extended-stamp.
      See cx_read_stamp in builtins.cc for details
      If an object has an extended-stamp then the extended-stamp is used for fastgf generic function
      dispatch and stamp is used by MPS to determine the object layout and C++ IsA relationships.
      Each time a standard-class is redefined a new STAMP is generated and that
      is later written into the Instance_O rack.

      The header ends with a uintptr_t additional_data[0], an array of uintptr_t which intrudes
      into the client data and is used when some header tags (fwd, pad) need it.
      NOTE!!!   Writing anything into header.additional_data[0] wipes out the objects vtable and completely
                invalidates it - this is only used by the MPS GC when it's ok to invalidate the object.
    */

class BaseHeader_s {
public:
  // fixme2022
  static const size_t mtag_shift = 3; // mtags are 3 bits wide
  static const tagged_stamp_t mtag_mask = 0b111;
#if 1
  static const size_t general_mtag_width = mtag_shift;
  static const tagged_stamp_t general_mtag_mask = 0b111;
#else
  static const size_t general_mtag_width = 2;
  static const tagged_stamp_t general_mtag_mask = 0b11;
#endif
  static const size_t general_mtag_shift = general_mtag_width; // MUST ALWAYS BE >=2 to match Fixnum shift
  /*!
   * It is important that weak_mtag is the same value as CHARACTER_TAG so that
   * it looks like an immediate value in AWL pools and doesn't break the
   * MPS invariant that only valid tagged pointers are allowed in AWL pools.
   */
  static const tagged_stamp_t general_mtag = 0b000;
  static const tagged_stamp_t invalid0_mtag = 0b001;
  static const tagged_stamp_t weak_mtag = 0b010;
  static const tagged_stamp_t cons_mtag = 0b011;
  static const tagged_stamp_t invalid1_mtag = 0b100;
  static const tagged_stamp_t fwd_mtag = 0b101;
  static const tagged_stamp_t pad_mtag = 0b110;
  static const tagged_stamp_t pad1_mtag = 0b111;
  static const tagged_stamp_t stamp_mtag = general_mtag;
  static const tagged_stamp_t stamp_mask = ~(tagged_stamp_t)general_mtag_mask; // 0b11...111111111111000;
  static const tagged_stamp_t where_mask = 0b11 << general_mtag_shift;
  // These MUST match the wtags used in clasp-analyzer.lisp and scraper/code-generator.lisp
  static const tagged_stamp_t derivable_wtag = 0b00 << general_mtag_shift;
  static const tagged_stamp_t rack_wtag = 0b01 << general_mtag_shift;
  static const tagged_stamp_t wrapped_wtag = 0b10 << general_mtag_shift;
  static const tagged_stamp_t header_wtag = 0b11 << general_mtag_shift;
  static const tagged_stamp_t max_wtag = 0b11 << general_mtag_shift;
  static const tagged_stamp_t wtag_width = 2;
  static const size_t general_stamp_shift = general_mtag_width + wtag_width;

  // Must match the number of bits to describe where_mask from the 0th bit
  // This is the width of integer that llvm needs to represent the masked off part of a header stamp
  static const tagged_stamp_t where_tag_width = wtag_width + general_mtag_width; // I'm not sure if this is right

  // stamp_tag MUST be 00 so that stamps look like FIXNUMs
  //    static const int stamp_shift = general_mtag_shift;
  static const tagged_stamp_t largest_possible_stamp = stamp_mask >> general_mtag_shift;

  //
  // Restrict stamps to specific ranges
  // Builtin classes are between 0...65536
  // clbind classes are between 65537...131072
  // Lisp classes are 131073 and higher
  static const size_t max_builtin_stamp = 65536;
  static const size_t max_clbind_stamp = 65536 + max_builtin_stamp;

public:
  //
  // Type of badge
  //
  typedef uint32_t badge_t;

  typedef enum {
    WeakBucketKind = ((1 << mtag_shift) | weak_mtag),
    StrongBucketKind = ((2 << mtag_shift) | weak_mtag),
    WeakMappingKind = ((3 << mtag_shift) | weak_mtag),
    StrongMappingKind = ((4 << mtag_shift) | weak_mtag),
    MaxWeakKind = ((5 << mtag_shift) | weak_mtag)
  } WeakKinds;

  //
  //
  struct Dummy_s {};
  struct StampWtagMtag {
    typedef tagged_stamp_t Value;
    uintptr_t _header_data[0]; // The 0th element overlaps StampWtagMtag values
    tagged_stamp_t _value;
    StampWtagMtag() : _value(0){};
    StampWtagMtag(Value all) : _value(all){};
    StampWtagMtag(WeakKinds kind) : _value(kind){};

    // WHAT IS GOING ON
    //      StampWtagMtag(UnshiftedStamp stamp, badge_t badge) : _value(shift_unshifted_stamp(stamp)), _header_badge(badge) {};

    // This is so we can find where we shift/unshift/don'tshift
    static UnshiftedStamp first_NextUnshiftedStamp(UnshiftedStamp start) {
      return (start + (1 << general_mtag_shift)) & (~(uintptr_t)general_mtag_mask);
    }
    uintptr_t stamp() const { return this->_value >> (wtag_width + general_mtag_width); };
    static bool is_unshifted_stamp(uint64_t unknown) {
      size_t sm = STAMPWTAG_max;
      // This is the only test that makes sense.
      if (is_header_stamp(unknown) && unknown <= sm)
        return true;
      // Otherwise it's an assigned stamp and it must be in the range below.
      if (sm < unknown && unknown < global_NextUnshiftedStamp)
        return true;
      return false;
    }

    Fixnum as_fixnum() const { return (Fixnum)this->_value; }

    static bool is_shifted_stamp(uint64_t unknown) {
      return (unknown & general_mtag_mask) == general_mtag; // mtag must be zero
    }
    static bool is_header_shifted_stamp(uint64_t unknown) {
      if ((unknown & general_mtag_mask) != general_mtag)
        return false;
      uint64_t stamp = unshift_shifted_stamp(unknown);
      if ((unknown & where_mask) == header_wtag) {
        return (stamp <= STAMPWTAG_max);
      }
      if ((unknown & where_mask) == rack_wtag) {
        return (stamp == STAMPWTAG_core__Instance_O || stamp == STAMPWTAG_core__FuncallableInstance_O ||
                stamp == STAMPWTAG_clbind__ClassRep_O);
      }
      if ((unknown & where_mask) == wrapped_wtag) {
        return (stamp == STAMPWTAG_core__WrappedPointer_O);
      }
      return (stamp == STAMPWTAG_core__DerivableCxxObject_O);
    }
    static bool is_rack_shifted_stamp(uint64_t header_word) {
      return ((header_word & general_mtag_mask) == general_mtag) &&
             ((header_word & where_mask) == rack_wtag); // mtag must be zero and wtag = rack_wtag
    }
    static bool is_wrapped_shifted_stamp(uint64_t header_word) {
      return ((header_word & general_mtag_mask) == general_mtag) &&
             ((header_word & where_mask) == wrapped_wtag); // mtag must be zero and wtag = wrapped_wtag
    }
    static bool is_derivable_shifted_stamp(uint64_t header_word) {
      return ((header_word & general_mtag_mask) == general_mtag) &&
             ((header_word & where_mask) == derivable_wtag); // mtag must be zero and wtag = derivable_wtag
    }
    static bool is_header_stamp(uint64_t header_word) { return is_header_shifted_stamp(shift_unshifted_stamp(header_word)); }
    static bool is_rack_stamp(uint64_t header_word) { return is_rack_shifted_stamp(shift_unshifted_stamp(header_word)); }
    static bool is_wrapped_stamp(uint64_t header_word) { return is_wrapped_shifted_stamp(shift_unshifted_stamp(header_word)); }
    static bool is_derivable_stamp(uint64_t header_word) { return is_derivable_shifted_stamp(shift_unshifted_stamp(header_word)); }
    static ShiftedStamp shift_unshifted_stamp(UnshiftedStamp us) {
      return ((us << BaseHeader_s::general_mtag_shift) | BaseHeader_s::general_mtag);
    }
    static size_t make_nowhere_stamp(UnshiftedStamp us) {
      // Remove the where part of the unshifted stamp
      // The resulting value will be unique to the class and adjacent to each other
      //    andsuitable for indices into an array
      return (size_t)(us >> wtag_width);
    }
    static size_t get_stamp_where(UnshiftedStamp us) { return (size_t)((us & where_mask) >> general_mtag_shift); }
    static UnshiftedStamp unshift_shifted_stamp(ShiftedStamp us) { return ((us >> BaseHeader_s::general_mtag_shift)); }

    template <typename T> static StampWtagMtag make() {
      StampWtagMtag mak((GCStamp<T>::StampWtag << general_mtag_shift));
      return mak;
    }
    static StampWtagMtag make_unknown(UnshiftedStamp the_stamp) { return the_stamp << general_mtag_shift; }
    static StampWtagMtag make_StampWtagMtag(StampWtagMtag vvv) {
      StampWtagMtag mak(vvv);
      return mak;
    }

  public:
    inline size_t mtag() const { return (size_t)(this->_value & mtag_mask); };
    bool invalidP() const {
      if (!(this->_value & general_mtag_mask))
        return false;
      tagged_stamp_t val = (this->_value & general_mtag_mask);
      return (val == invalid0_mtag) || (val == invalid1_mtag);
    };
    bool stampP() const { return (this->_value & general_mtag_mask) == general_mtag; };
    bool generalObjectP() const { return this->stampP(); };
    bool weakObjectP() const { return (this->_value & mtag_mask) == weak_mtag; };
    bool consObjectP() const { return (this->_value & mtag_mask) == cons_mtag; };
    bool fwdP() const { return (this->_value & mtag_mask) == fwd_mtag; };
    bool fwdV() const { return (this->_value & mtag_mask); };
    bool padP() const { return (this->_value & mtag_mask) == pad_mtag; };
    bool pad1P() const { return (this->_value & mtag_mask) == pad1_mtag; };
    bool anyPadP() const { return this->padP() || this->pad1P(); };
    /*! No sanity checking done - this function assumes kindP == true */
    GCStampEnum stamp_wtag() const { return (GCStampEnum)(this->_value >> general_mtag_shift); };
    GCStampEnum stamp_() const { return (GCStampEnum)(this->_value >> (wtag_width + general_mtag_shift)); };
    /*! No sanity checking done - this function assumes fwdP == true */
    void* fwdPointer() const { return reinterpret_cast<void*>(this->_header_data[0] & (~(uintptr_t)mtag_mask)); };
    /*! Return the size of the fwd block - without the header. This reaches into the client area to get the size */
    void setFwdPointer(void* ptr) { this->_header_data[0] = reinterpret_cast<uintptr_t>(ptr) | fwd_mtag; };
    uintptr_t fwdSize() const { return this->_header_data[1]; };
    /*! This writes into the first tagged_stamp_t sized word of the client data. */
    void setFwdSize(size_t sz) { this->_header_data[1] = sz; };
    /*! Define the header as a pad, pass pad_tag or pad1_tag */
    void setPad(tagged_stamp_t p) { this->_header_data[0] = p; };
    /*! Return the pad1 size */
    tagged_stamp_t pad1Size() const { return Alignment(); };
    /*! Return the size of the pad block - without the header */
    tagged_stamp_t padSize() const { return (this->_header_data[1]); };
    /*! This writes into the first tagged_stamp_t sized word of the client data. */
    void setPadSize(size_t sz) { this->_header_data[1] = sz; };

  public:
    // GenerateHeaderValue must be passed to make_fixnum and the result exactly matches a header value
    //    Converts a STAMP_WTAG into what it would look like if it was a Fixnum_sp
    template <typename T> static int64_t GenerateHeaderValue() {
      return (int64_t)(GCStamp<T>::StampWtag) << (general_mtag_shift - fixnum_shift);
    };
    //
    //
    // This must coordinate with what core::instance-stamp returns
    //     and what core__shift_stamp_for_compiled_code returns
    template <typename T> static int64_t GenerateTypeqHeaderValue() {
      return (int64_t)(GCStamp<T>::StampWtag << general_mtag_shift - fixnum_shift);
    };

  public: // header readers
    inline UnshiftedStamp unshifted_stamp() const {
      //        printf("%s:%d  unshifted_stamp() this->_value -> %lu\n", __FILE__, __LINE__, this->_value);
      return static_cast<UnshiftedStamp>(unshift_shifted_stamp(this->_value));
    }
    inline size_t nowhere_stamp() const {
      size_t us = this->unshifted_stamp();
      // printf("%s:%d  unshifted_stamp -> %lu\n", __FILE__, __LINE__, us);
      return make_nowhere_stamp(us);
    }
  };

  struct BadgeStampWtagMtag : public StampWtagMtag {
    static constexpr badge_t IllegalBadge = ILLEGAL_BADGE;
    static constexpr badge_t NoBadge = NO_BADGE;

    /* _header_badge starts out being NoBadge but when the object is hashed - then it is assigned a badge
        that is not IllegalBadge or NoBadge.   This defers random number generation for hashing until it is
        needed.
     */
    mutable std::atomic<badge_t> _header_badge; /* This can NEVER be zero or the boehm mark procedures in precise mode */
                                                /* will treat it like a unused object residing on a free list          */

    BadgeStampWtagMtag() : StampWtagMtag(), _header_badge(NoBadge){};
    BadgeStampWtagMtag(core::Cons_O* cons) : StampWtagMtag(cons_mtag), _header_badge((badge_t)((uintptr_t)cons & 0xFFFFFFFF)){};
    BadgeStampWtagMtag(const BadgeStampWtagMtag& other);
    BadgeStampWtagMtag(StampWtagMtag all) : StampWtagMtag(all), _header_badge(NoBadge){};
    BadgeStampWtagMtag(StampWtagMtag all, badge_t badge) : StampWtagMtag(all), _header_badge(badge) {
#ifdef DEBUG_BADGE_SSL
      if (badge > 1) {
        printf("%s:%d:%s this = %p badge = %u   _header_badge = %u\n", __FILE__, __LINE__, __FUNCTION__, (void*)this, badge,
               this->_header_badge.load());
      }
#endif
    };
    BadgeStampWtagMtag(WeakKinds kind) : StampWtagMtag(kind), _header_badge((uintptr_t)this & 0xFFFFFFFF){};
  };

  static inline int Stamp(StampWtagMtag stamp_wtag_mtag) {
    GCTOOLS_ASSERT((stamp_wtag_mtag._value & general_mtag_mask) == general_mtag);
    return stamp_wtag_mtag._value >> (wtag_width + general_mtag_shift);
  }
  //
  //
  // End of StampWtagMtag
  //
  //

public:
  static void signal_invalid_object(const BaseHeader_s* header, const char* msg);

public:
  void validate() const;
  void quick_validate() const {
#ifdef DEBUG_QUICK_VALIDATE
    if (this->stampP()) {
      if (!is_unshifted_stamp(this->unshifted_stamp()))
        signal_invalid_object(this, "bad kind");
    }
#else
    this->validate();
#endif
  }

public:
  // The header contains the stamp_wtag_mtag value.
  BadgeStampWtagMtag _badge_stamp_wtag_mtag; // This MUST be the first word of the guard.
public:
  BaseHeader_s(const StampWtagMtag& k, bool dummy) : _badge_stamp_wtag_mtag(k) {
#ifdef DEBUG_BADGE_SSL
    if (dummy)
      printf("%s:%d:%s    this = %p   badge = %u\n", __FILE__, __LINE__, __FUNCTION__, (void*)this,
             this->_badge_stamp_wtag_mtag._header_badge.load());
#endif
  }
  BaseHeader_s(const BadgeStampWtagMtag& k, bool dummy) : _badge_stamp_wtag_mtag(k) {
#ifdef DEBUG_BADGE_SSL
    if (dummy)
      printf("%s:%d:%s    this = %p   badge = %u\n", __FILE__, __LINE__, __FUNCTION__, (void*)this,
             this->_badge_stamp_wtag_mtag._header_badge.load());
#endif
  }
  BaseHeader_s(const StampWtagMtag& k) : _badge_stamp_wtag_mtag(k) {}
  BaseHeader_s(const BaseHeader_s& baseHeader);

public:
  size_t mtag() const { return (size_t)(this->_badge_stamp_wtag_mtag._value & mtag_mask); };
  constexpr size_t tail_size() const { return 0; };

  ShiftedStamp shifted_stamp() const { return (ShiftedStamp)(this->_badge_stamp_wtag_mtag._value); };

  string description() const {
    if (this->_badge_stamp_wtag_mtag.stampP()) {
      std::stringstream ss;
      ss << "Header=" << (void*)(uintptr_t)(this->_badge_stamp_wtag_mtag._value);
      ss << "/";
      ss << obj_name(this->_badge_stamp_wtag_mtag.stamp_());
      return ss.str();
    } else if (this->_badge_stamp_wtag_mtag.consObjectP()) {
      return "Header_CONS";
    } else if (this->_badge_stamp_wtag_mtag.weakObjectP()) {
      return "Header_WEAK";
    } else if (this->_badge_stamp_wtag_mtag.fwdP()) {
      std::stringstream ss;
      ss << "Fwd/ptr=" << this->_badge_stamp_wtag_mtag.fwdPointer() << "/sz=" << this->_badge_stamp_wtag_mtag.fwdSize();
      return ss.str();
    } else if (this->_badge_stamp_wtag_mtag.pad1P()) {
      return "Pad1";
    } else if (this->_badge_stamp_wtag_mtag.padP()) {
      stringstream ss;
      ss << "Pad/sz=" << this->_badge_stamp_wtag_mtag.padSize();
      return ss.str();
    }
    stringstream ss;
    ss << "IllegalHeader=";
    ss << (void*)(uintptr_t)(this->_badge_stamp_wtag_mtag._value);
    printf("%s:%d Header->description() found an illegal header = %s\n", __FILE__, __LINE__, ss.str().c_str());
    return ss.str();
    ;
  }

#ifdef USE_PRECISE_GC
  //
  // When USE_PRECISE_GC we can determine more about objects
  //
  bool preciseIsPolymorphic() const;
#endif
};

struct GatherObjects; // forward decl

class ConsHeader_s : public BaseHeader_s {
public:
  ConsHeader_s(const BadgeStampWtagMtag& k) : BaseHeader_s(k){};

public:
  static constexpr size_t size() { return sizeof(ConsHeader_s); };
  bool isValidConsObject(GatherObjects* gather) const;
};

class Header_s : public BaseHeader_s {
public:
#ifdef DEBUG_GUARD
  int _tail_start;
  int _tail_size;
  uintptr_t _guard;
  uintptr_t _source;
  uint _guard2;
#endif
#ifdef DEBUG_GUARD_BACKTRACE
  void* _backtrace[GUARD_BACKTRACE_LEVELS];
#endif
#ifdef DEBUG_GUARD
  // The last word of the guard must be a copy of the first.
  //  this is so that we can get the stamp_wtag_mtag by subtracting
  //  from the client pointer AND we can get it from a header pointer.
  BadgeStampWtagMtag _dup_badge_stamp_wtag_mtag; // This MUST be the last word of the guard.
#endif

public:
  bool isValidGeneralObject(GatherObjects* gather) const;
  void validate() const;
  void quick_validate() const {
#ifdef DEBUG_QUICK_VALIDATE
    if (this->stampP()) {
#ifdef DEBUG_GUARD
      if (this->_guard != 0xFEEAFEEBDEADBEEF)
        signal_invalid_object(this, "bad head guard");
      if (this->_tail_size > 0) {
        const unsigned char* tail = (const unsigned char*)this + this->_tail_start;
        if ((*tail) != 0xcc)
          signal_invalid_object(this, "bad tail not 0xcc");
      }
#endif
      if (!is_unshifted_stamp(this->unshifted_stamp()))
        signal_invalid_object(this, "bad kind");
    }
#else
    this->validate();
#endif
  }

#define GUARD_BACKTRACE_LEVELS 8
#ifdef DEBUG_GUARD_BACKTRACE
  inline void maybe_fill_backtrace(tagged_stamp_t k) {
    void** bp = (void**)__builtin_frame_address(0);
    void** bpnew;
    void* pc;
    for (size_t ii = 0; ii < GUARD_BACKTRACE_LEVELS; ++ii) {
      pc = *(void**)(bp + 1);
      bpnew = (void**)*(void**)bp;
      if ((uintptr_t)bpnew < (uintptr_t)bp)
        break;
      if (bpnew > my_thread_low_level->_StackTop)
        break;
      bp = bpnew;
      this->_backtrace[ii] = pc;
    }
  };
#endif
#ifdef DEBUG_GUARD
  inline void fill_tail() {
    if (this->_tail_size)
      memset((void*)(((char*)this) + this->_tail_start), 0xcc, this->_tail_size);
  };
#endif

#if !defined(DEBUG_GUARD)
  Header_s(const BadgeStampWtagMtag& k) : BaseHeader_s(k){};
  Header_s(Header_s* headerptr) : BaseHeader_s(headerptr->_badge_stamp_wtag_mtag){};
  Header_s(Header_s* headerptr, bool verbose) : BaseHeader_s(headerptr->_badge_stamp_wtag_mtag, verbose) {
#ifdef DEBUG_BADGE_SSL
    if (verbose)
      printf("%s:%d:%s    this = %p   headerptr badge = %u   badge = %u\n", __FILE__, __LINE__, __FUNCTION__, (void*)this,
             headerptr->_badge_stamp_wtag_mtag._header_badge.load(), this->_badge_stamp_wtag_mtag._header_badge.load());
#endif
  };
#else
#define GUARD1 0xFEEAFEEBDEADBEE0
#define GUARD2 0xC0FFEEE0

  Header_s(const BadgeStampWtagMtag& k, size_t tstart = 0, size_t tsize = 0, size_t total_size = sizeof(Header_s))
      : BaseHeader_s(k), _guard(GUARD1), _tail_start(tstart), _tail_size(tsize), _source((uintptr_t)this), _guard2(GUARD2),
        _dup_badge_stamp_wtag_mtag(k) {
#ifdef DEBUG_GUARD_BACKTRACE
    this->maybe_fill_backtrace(k._value);
#endif
    this->fill_tail();
  };

  Header_s(Header_s* headerptr)
      : BaseHeader_s(headerptr->_badge_stamp_wtag_mtag), _guard(GUARD1), _tail_start(0), _tail_size(0), _source(headerptr->_source),
        _guard2(GUARD2), _dup_badge_stamp_wtag_mtag(headerptr->_badge_stamp_wtag_mtag){};
  Header_s(Header_s* headerptr, bool verbose)
    : BaseHeader_s(headerptr->_badge_stamp_wtag_mtag, verbose), _guard(GUARD1),
      _tail_start(0), _tail_size(0), _source(headerptr->_source),
      _guard2(GUARD2), _dup_badge_stamp_wtag_mtag(headerptr->_badge_stamp_wtag_mtag)
  {
#ifdef DEBUG_BADGE_SSL
    if (verbose)
      printf("%s:%d:%s    this = %p   headerptr badge = %u   badge = %u\n", __FILE__, __LINE__, __FUNCTION__, (void*)this,
             headerptr->_badge_stamp_wtag_mtag._header_badge.load(), this->_badge_stamp_wtag_mtag._header_badge.load());
#endif
  }

#endif

#ifdef DEBUG_GUARD
  size_t tail_size() const { return this->_tail_size; };
#endif
};

template <class LispClass> struct StackAllocate {
  Header_s _Header;
  LispClass _Object;

  template <class... ARGS>
  StackAllocate(ARGS&&... args) : _Header(Header_s::StampWtagMtag::make<LispClass>()), _Object(std::forward<ARGS>(args)...){};

  smart_ptr<LispClass> asSmartPtr() { return smart_ptr<LispClass>((LispClass*)&this->_Object); }
};

// We use a sham struct because C++ doesn't let us partially specify templates.
template <class LispClass> struct InitializeObject {

  static size_t size() { return sizeof_with_header<LispClass>(); }

  template <typename... ARGS> static LispClass* go(void* where, ARGS&&... args) {
    LispClass* object = (LispClass*)((Header_s*)where + 1);
    new (where) Header_s(Header_s::StampWtagMtag::make<LispClass>());
    return new (object) LispClass(std::forward<ARGS>(args)...);
  }
};

template <> struct InitializeObject<core::Cons_O> {

  // KLUDGE: We can't specialize InitializeObject directly because Cons_O
  // is not defined enough yet that we can write code to initialize one.
  // But if we put in another layer of template, it's apparently okay.
  template <class ConsType, typename... ARGS> static ConsType* initialize_cons(void* where, ARGS&&... args) {
    ConsType* object = (ConsType*)((ConsHeader_s*)where + 1);
    new (where) ConsHeader_s(ConsHeader_s::StampWtagMtag::make<ConsType>());
    return new (object) ConsType(std::forward<ARGS>(args)...);
  }

  template <typename... ARGS> static core::Cons_O* go(void* where, ARGS&&... args) {
    return initialize_cons<core::Cons_O>(where, std::forward<ARGS>(args)...);
  }
};

}; // namespace gctools

// ------------------------------------------------------------
//
// Stamp
//

namespace gctools {
/* NextUnshiftedStamp(...) returns a unique Stamp value every time it is called.
   They are generated when creating and redefining classes and
   must be unique system-wide.  They are used for generic function dispatch.
*/

/*! global_NextBuiltInStamp starts at STAMPWTAG_max+1
    See definition in memoryManagement.cc
    This is so that it doesn't use any stamps that were set by the static analyzer. */
extern std::atomic<UnshiftedStamp> global_NextUnshiftedStamp;
extern std::atomic<UnshiftedStamp> global_NextUnshiftedClbindStamp;
/*! Return a new stamp for BuiltIn classes.
    If given != STAMPWTAG_null then simply return give as the stamp.
    Otherwise return the global_NextBuiltInStamp and advance it
    to the next one */
void OutOfStamps();
inline ShiftedStamp NextStampWtag(ShiftedStamp where, UnshiftedStamp given = STAMPWTAG_null) {
  if (given != STAMPWTAG_null) {
    return Header_s::StampWtagMtag::shift_unshifted_stamp(given) | where;
  }
  UnshiftedStamp stamp = global_NextUnshiftedStamp.fetch_add(1 << Header_s::wtag_width);
  if (stamp < Header_s::largest_possible_stamp) {
#ifdef DEBUG_ASSERT
    if (!(Header_s::StampWtagMtag::is_unshifted_stamp(stamp)) && (stamp & Header_s::where_mask) != 0) {
      printf("%s:%d NextStampWtag is about to return a stamp that is illegal: stamp: %lu\n", __FILE__, __LINE__, stamp);
    }
#endif
    return Header_s::StampWtagMtag::shift_unshifted_stamp(stamp) | where;
  }
  OutOfStamps();
  abort();
}
void OutOfClbindStamps();
inline ShiftedStamp NextClbindStampWtag(ShiftedStamp where, UnshiftedStamp given = STAMPWTAG_null) {
  if (given != STAMPWTAG_null) {
    return Header_s::StampWtagMtag::shift_unshifted_stamp(given) | where;
  }
  UnshiftedStamp stamp = global_NextUnshiftedClbindStamp.fetch_add(1 << Header_s::wtag_width);
  if (stamp < Header_s::max_clbind_stamp) {
#ifdef DEBUG_ASSERT
    if (!(Header_s::StampWtagMtag::is_unshifted_stamp(stamp)) && (stamp & Header_s::where_mask) != 0) {
      printf("%s:%d NextStampWtag is about to return a stamp that is illegal: stamp: %lu\n", __FILE__, __LINE__, stamp);
    }
#endif
    //      printf("%s:%d:%s assigned clbind stamp: %lu\n", __FILE__, __LINE__, __FUNCTION__ , stamp);
    return Header_s::StampWtagMtag::shift_unshifted_stamp(stamp) | where;
  }
  OutOfClbindStamps();
  abort();
}
/*!
 * All builtin stamps have been assigned
 * prepare the system to assign Clbind stamps and lisp stamps.
 */
void FinishAssigningBuiltinStamps();

}; // namespace gctools

namespace gctools {
#ifdef DEBUG_GUARD_VALIDATE
#define EXHAUSTIVE_VALIDATE(ptr) (ptr)->quick_validate();
#else
#define EXHAUSTIVE_VALIDATE(ptr)
#endif

inline const void* GeneralPtrToHeaderPtr(const void* mostDerived) {
  const void* ptr = reinterpret_cast<const char*>(mostDerived) - sizeof(Header_s);
  return ptr;
}

inline constexpr size_t SizeofGeneralHeader() { return sizeof(Header_s); };

inline void* GeneralPtrToHeaderPtr(void* mostDerived) {
  void* ptr = reinterpret_cast<char*>(mostDerived) - SizeofGeneralHeader();
  return ptr;
}

inline const Header_s* header_pointer(const void* client_pointer) {
  const Header_s* header = reinterpret_cast<const Header_s*>(reinterpret_cast<const char*>(client_pointer) - sizeof(Header_s));
  return header;
}

inline void throwIfInvalidClient(core::T_O* client) {
  Header_s* header = (Header_s*)GeneralPtrToHeaderPtr(client);
  if (header->_badge_stamp_wtag_mtag.invalidP()) {
    throw_hard_error_bad_client((void*)client);
  }
}

template <typename T> inline T* HeaderPtrToGeneralPtr(void* base) {
  T* ptr = reinterpret_cast<T*>(reinterpret_cast<char*>(base) + SizeofGeneralHeader());
  return ptr;
}

/*
 * This must ALWAYS be the same as SizeofGeneralHeader
 */
inline constexpr size_t SizeofWeakHeader() { return SizeofGeneralHeader(); };

inline const void* WeakPtrToHeaderPtr(const void* client) {
  const void* ptr = reinterpret_cast<const char*>(client) - SizeofWeakHeader();
  return ptr;
}

inline void* WeakPtrToHeaderPtr(void* client) {
  void* ptr = reinterpret_cast<char*>(client) - SizeofWeakHeader();
  return ptr;
}

inline void* HeaderPtrToWeakPtr(void* header) {
  void* ptr = reinterpret_cast<void*>(reinterpret_cast<char*>(header) + SizeofWeakHeader());
  return ptr;
}

inline constexpr size_t SizeofConsHeader() { return ConsHeader_s::size(); };

inline const void* ConsPtrToHeaderPtr(const void* client) {
  const void* ptr = reinterpret_cast<const char*>(client) - SizeofConsHeader();
  return ptr;
}

inline void* ConsPtrToHeaderPtr(void* client) {
  void* ptr = reinterpret_cast<char*>(client) - SizeofConsHeader();
  return ptr;
}

inline void* HeaderPtrToConsPtr(void* header) {
  void* ptr = reinterpret_cast<void*>(reinterpret_cast<char*>(header) + SizeofConsHeader());
  return ptr;
}

}; // namespace gctools

#include <clasp/gctools/cast.h>
#include <clasp/gctools/tagged_cast.h>

namespace gctools {

// ----------------------------------------------------------------------
//! Calculate the size of an object + header for allocation
template <class T> inline size_t sizeof_with_header() { return AlignUp(sizeof(T)) + sizeof(Header_s); }

/*! Size of containers given the number of elements */
template <typename Cont_impl> size_t sizeof_container(size_t n) {
  size_t classSz = sizeof(Cont_impl);
  size_t dataSz = sizeof(typename Cont_impl::value_type) * n;
  size_t totalSz = classSz + dataSz;
  return AlignUp(totalSz);
};

/*
 * atomic == Object contains no internal tagged pointers, is collectable
 * normal == Object contains internal tagged pointers, is collectable
 * collectable_immobile == Object cannot be moved but is collectable
 * unmanaged == Object cannot be moved and cannot be automatically collected
 */
typedef enum { atomic = 0, normal = 1, collectable_immobile = 2, unmanaged = 3 } GCInfo_policy;

template <class OT> struct GCInfo {
  static bool const NeedsInitialization = true; // Currently, by default,  everything needs initialization
  static bool const NeedsFinalization = false;  // By default, nothing needs finalization
  static constexpr GCInfo_policy Policy = normal;
};

template <class T> inline size_t sizeof_container_with_header(size_t num) { return sizeof_container<T>(num) + sizeof(Header_s); };

/*! Size of containers given the number of binits where BinitWidth is the number of bits/bunit */
template <typename Cont_impl> size_t sizeof_bitunit_container(size_t n) {
  size_t classSz = sizeof(Cont_impl);
  size_t dataSz = Cont_impl::bitunit_array_type::sizeof_for_length(n);
  size_t totalSz = classSz + dataSz;
  size_t aligned_totalSz = AlignUp(totalSz);
#ifdef DEBUG_BITUNIT_CONTAINER
  printf("%s:%d classSz = %lu\n", __FILE__, __LINE__, classSz);
  printf("%s:%d dataSz = %lu\n", __FILE__, __LINE__, dataSz);
  printf("%s:%d totalSz = %lu\n", __FILE__, __LINE__, totalSz);
  printf("%s:%d aligned_totalSz = %lu\n", __FILE__, __LINE__, aligned_totalSz);
#endif
  return aligned_totalSz;
};

template <class T> inline size_t sizeof_bitunit_container_with_header(size_t num) {
  size_t size_bitunit_container = sizeof_bitunit_container<T>(num);
  size_t size_header = sizeof(Header_s);
  size_t sum = size_bitunit_container + size_header;
#ifdef DEBUG_BITUNIT_CONTAINER
  printf("%s:%d  In sizeof_bitunit_container_with_header  num = %lu\n", __FILE__, __LINE__, num);
  printf("%s:%d  In sizeof_bitunit_container_with_header   size_bitunit_container = %lu\n", __FILE__, __LINE__,
         size_bitunit_container);
  printf("%s:%d  In sizeof_bitunit_container_with_header   sum = %lu\n", __FILE__, __LINE__, sum);
#endif
  return sum;
};

/* Align size upwards and ensure that it's big enough to store a
 * forwarding pointer.
 * This is used by the obj_scan and obj_skip methods
 */
/*   Replaces this macro...
     #define ALIGN(size)                                                \
    (AlignUp<Header_s>(size) >= AlignUp<Header_s>(sizeof_with_header<gctools::Fwd_s>())	\
     ? AlignUp<Header_s>(size)                              \
     : gctools::sizeof_with_header<gctools::Fwd_s>() )
*/

extern size_t global_sizeof_fwd;
inline size_t Align(size_t size) { return ((AlignUp(size) >= global_sizeof_fwd) ? AlignUp(size) : global_sizeof_fwd); };

// Manually define these for the sake of As<Fixnum_sp> etc.
template <> struct GCStamp<core::Fixnum_I> {
public:
  static GCStampEnum const StampWtag = STAMPWTAG_core__Fixnum_dummy_O;
};
template <> struct GCStamp<core::SingleFloat_I> {
public:
  static GCStampEnum const StampWtag = STAMPWTAG_core__SingleFloat_dummy_O;
};
template <> struct GCStamp<core::Character_I> {
public:
  static GCStampEnum const StampWtag = STAMPWTAG_core__Character_dummy_O;
};

}; // namespace gctools

#if defined(USE_BOEHM)
#define NON_MOVING_GC 1
#include <clasp/gctools/boehmGarbageCollection.h>
#elif defined(USE_MMTK)
#include <clasp/gctools/mmtkGarbageCollection.h>
#elif defined(USE_MPS)
#include <clasp/gctools/mpsGarbageCollection.h>
#endif

extern "C" {
const char* obj_name(gctools::stamp_t kind);
const char* obj_kind_name(core::T_O* ptr);
size_t obj_kind(core::T_O* ptr);
extern void obj_dump_base(void* base);
};

namespace gctools {
/*! Specialize GcKindSelector so that it returns the appropriate GcKindEnum for OT */
template <class OT> struct GCStamp {
#ifndef USE_GC_PRECISE
  // Only define this when not using precise gc
  static GCStampEnum const StampWtag = STAMPWTAG_null;
#endif
};
}; // namespace gctools

// ------------------------------------------------------------
//
// Specializations when running boehmdc or the static analyzer
//
namespace core {
class Fixnum_dummy_O;
class SingleFloat_dummy_O;
class Character_dummy_O;
class CPointer_dummy_O;
class Cons_O;
class Vaslist_dummy_O;
class Instance_O;
class FuncallableInstance_O;
} // namespace core

namespace gctools {};

#include <clasp/gctools/smart_pointers.h>

#include <clasp/core/coretypes.h>

namespace core {
typedef gctools::smart_ptr<Cons_O> Cons_sp;
typedef gctools::smart_ptr<General_O> General_sp;
typedef gctools::smart_ptr<T_O> T_sp;
}; // namespace core

namespace gctools {
uint32_t lisp_calculate_heap_badge();
uint32_t lisp_general_badge(core::General_sp object);
uint32_t lisp_cons_badge(core::Cons_sp object);
uint32_t lisp_badge(core::T_sp object);
}; // namespace gctools

namespace gctools {
extern int global_pollTicksPerCleanup;

template <typename T> void* SmartPtrToBasePtr(smart_ptr<T> obj) {
  void* ptr;
  if (obj.objectp()) {
    ptr = reinterpret_cast<void*>(reinterpret_cast<char*>(obj.untag_object()) - sizeof(Header_s));
  } else {
    throw_hard_error("Bad pointer for SmartPtrToBasePtr");
  }
  return ptr;
}
}; // namespace gctools

#include <clasp/gctools/gcStack.h>
// #include <clasp/gctools/gcalloc.h>

/*! These don't do anything at the moment
  but may be used in the future to create unsafe-gc points
*/

#define SUPPRESS_GC()                                                                                                              \
  {}
#define ENABLE_GC()                                                                                                                \
  {}

namespace gctools {

int handleFatalCondition();

void rawHeaderDescribe(const uintptr_t* headerP);
}; // namespace gctools

extern "C" {
// These must be provided the the garbage collector specific code

//! Describe the header of the client
void client_describe(void* taggedClient);
//! Validate the client
void client_validate_tagged(gctools::Tagged taggedClient);
//! Must be a General_O ptr - no tag
void client_validate_General_O_ptr(const core::General_O* client_ptr);
//! Validate a client smart_ptr - only general objects
void client_validate(core::T_sp client);
//! Describe the header
void header_describe(gctools::Header_s* headerP);
};

// #include <clasp/gctools/containers.h>

namespace gctools {
#define LITERAL_TAG_CHAR 0
#define TRANSIENT_TAG_CHAR 1

void untag_literal_index(size_t findex, size_t& index, size_t& tag);
}; // namespace gctools

namespace gctools {

/*! Maintains pointers to arrays of roots that are stored in LLVM Modules
    we add and remove during runtime as Modules are compiled and (in the future) removed.
 */

struct GCRootsInModule {
  static int const TransientRawIndex = 0;
  static size_t const DefaultCapacity = 256;
  // Fields
  core::SimpleVector_O** _TransientAlloca;
  void* _module_memory;
  size_t _num_entries;
  size_t _capacity;
  /*fnLispCallingConvention* */ void** _function_pointers;
  size_t _function_pointer_count;
  GCRootsInModule(void* module_mem, size_t num_entries, core::SimpleVector_O** transient_alloca, size_t transient_entries,
                  size_t function_pointer_count, void** fptrs);
  void setup_transients(core::SimpleVector_O** transient_alloca, size_t transient_entries);

  size_t remainingCapacity() { return this->_capacity - this->_num_entries; };
  size_t push_back(Tagged val);
  Tagged setLiteral(size_t index, Tagged val);
  Tagged getLiteral(size_t index);
  Tagged setTransient(size_t index, Tagged val);
  Tagged getTransient(size_t index);
  Tagged setTaggedIndex(char tag, size_t index, Tagged val);
  Tagged getTaggedIndex(char tag, size_t index);
  /*fnLispCallingConvention*/ void* lookup_function(size_t index);
  void* address(size_t index) { return reinterpret_cast<void*>(&reinterpret_cast<core::T_sp*>(this->_module_memory)[index + 1]); }
};

void initialize_gcroots_in_module(GCRootsInModule* gcroots_in_module, core::T_O** root_address, size_t num_roots,
                                  gctools::Tagged initial_data, core::SimpleVector_O** transientAlloca, size_t transient_entries,
                                  size_t function_pointer_number, void** fptrs);
core::T_O* read_gcroots_in_module(GCRootsInModule* roots, size_t index);
void shutdown_gcroots_in_module(GCRootsInModule* gcroots_in_module);

inline core::T_O* ensure_valid_object(core::T_O* tagged_object) {
  // Only validate general objects for now
  if (tagged_generalp(tagged_object)) {
    core::T_O* untagged_object = gc::untag_general(tagged_object);
    Header_s* header = reinterpret_cast<Header_s*>(GeneralPtrToHeaderPtr(untagged_object));
    header->quick_validate();
  }
  return tagged_object;
}
inline void* ensure_valid_header(void* base) {
  // Only validate general objects for now
  Header_s* header = reinterpret_cast<Header_s*>(base);
  header->quick_validate();
  return base;
}

template <typename OT> inline gctools::smart_ptr<OT> ensure_valid_object(gctools::smart_ptr<OT> tagged_object) {
#ifdef DEBUG_GUARD_VALIDATE
  // Only validate general objects for now
  if (tagged_generalp(tagged_object.raw_())) {
    core::T_O* untagged_object = gc::untag_general(tagged_object.raw_());
    Header_s* header = reinterpret_cast<Header_s*>(GeneralPtrToHeaderPtr(untagged_object));
    header->quick_validate();
  }
#endif
  return tagged_object;
}
}; // namespace gctools

extern "C" {
// Invoke mps_park/mps_release or boehm_park/boehm_release
void gc_park();
void gc_release();
};

#ifdef DEBUG_GUARD_VALIDATE
#define ENSURE_VALID_OBJECT(x) (gctools::ensure_valid_object(x))
#define EVO(x) (gctools::ensure_valid_object(x))
#define ENSURE_VALID_HEADER(x) (gctools::ensure_valid_header(x))
#else
#define ENSURE_VALID_OBJECT(x) x
#define EVO(x)
#define ENSURE_VALID_HEADER(x) x
#endif

#ifdef DEBUG_THROW_IF_INVALID_CLIENT_ON
#define DEBUG_THROW_IF_INVALID_CLIENT(c) throwIfInvalidClient(reinterpret_cast<core::T_O*>(c))
#else
#define DEBUG_THROW_IF_INVALID_CLIENT(c)
#endif

namespace gctools {
struct SafeGCPark {
  SafeGCPark() { gc_park(); };
  ~SafeGCPark() { gc_release(); }
};
}; // namespace gctools

////////////////////////////////////////////////////////////
/*!
 * dont_expose<xxx>
 *
 * This is a template type that indicates that we do not want
 * to expose a variable of this type by the static analyzer.
 *
 *  This indicates to the static analyzer that the field
 *   shouldn't be recursively introspected further by the static
 *   analyzer and that it
 *   shouldn't be saved or loaded in image save/load.
 *   There can be many reasons for this:
 *     1. The type may contain private fields that we can't access.
 *     2. The type may contain pointers to C++ memory that we can't save/load.
 *
 *  The static analyzer will generate an entry in the clasp_gc_xxx.cc file
 *  for this that looks like...
 *    {  fixed_field, DONT_EXPOSE_OFFSET, sizeof(std::posix_time::ptime),
 *          __builtin_offsetof(SAFE_TYPE_MACRO(core::PosixTime_O),_Time),
 *          "_Time" }, // atomic: NIL public: (NIL) fixable: NIL good-name: T
 *
 *  So you can still get the offset and size of the field.
 *  The DONT_EXPOSE_OFFSET indicates that the containing object should
 *  NOT be saved to an image - because it will break the image save/load.
 *
 *  USAGE:
 *    1. Wrap the type of the field with dont_expose<XXX> where XXX is the type.
 *    2. Access the contents of the dont_expose<XXX> type using ._value
 */

template <typename Type> struct dont_expose {
  Type _value;
  dont_expose(){};
  template <typename Arg> dont_expose(const Arg& val) : _value(val){};
};

namespace gctools {

typedef void (*PointerFix)(uintptr_t* clientAddress, uintptr_t client, uintptr_t tag, void* user_data);
extern PointerFix globalMemoryWalkPointerFix;

struct MarkNode {
  gctools::Tagged* _ObjectAddr;
  MarkNode* _Next;
  MarkNode(gctools::Tagged* tt)
      : _ObjectAddr(tt), _Next(NULL){};
};

struct GatherObjects {
  RoomVerbosity _Verbosity;
  std::set<BaseHeader_s*> _Marked;
  MarkNode* _Stack;
  std::map<BaseHeader_s*, std::vector<uintptr_t>> _corruptObjects;
  size_t _SimpleFunCount;
  size_t _SimpleFunFailedDladdrCount;
  std::set<void*>  _uniqueEntryPoints;
  std::set<void*>  _uniqueEntryPointsFailedDladdr;
  GatherObjects(RoomVerbosity v) : _Verbosity(v), _Stack(NULL), _SimpleFunCount(0), _SimpleFunFailedDladdrCount(0) {};

  MarkNode* popMarkStack() {
    if (this->_Stack) {
      MarkNode* top = this->_Stack;
      this->_Stack = top->_Next;
      return top;
    }
    return NULL;
  }
  void pushMarkStack(MarkNode* node) {
    node->_Next = this->_Stack;
    this->_Stack = node;
  }

  void mark(Header_s* header) { this->_Marked.insert(header); }

  bool markedP(BaseHeader_s* header) { return this->_Marked.find(header) != this->_Marked.end(); }
};

void gatherAllObjects(GatherObjects& gather);
void mapAllObjects(void (*)(Tagged, void*), void*);
size_t objectSize(BaseHeader_s* header);

bool is_memory_readable(const void* address, size_t bytes = 8);

// Stuff for ROOM
// This struct holds info about a given class for ROOM, specifically
// how many instances of it there are, and how much memory those
// instances take up (in bytes).
struct ReachableClass {
  void update(size_t sz) {
    ++this->instances;
    this->totalSize += sz;
  };
  size_t instances = 0;
  size_t totalSize = 0;
};

typedef map<gctools::GCStampEnum, ReachableClass> ReachableClassMap;

// These four are GC-defined.
void fill_reachable_class_map(ReachableClassMap*);

size_t heap_size();
size_t free_bytes();
size_t bytes_since_gc();

}; // namespace gctools

extern "C" {
int startup_clasp(void** stackMarker, gctools::ClaspInfo* claspInfo, int* exitCode);

int run_clasp(gctools::ClaspInfo* claspInfo);

void shutdown_clasp(gctools::ClaspInfo* claspInfo);
};

namespace core {
extern size_t global_compile_discriminating_function_trigger;

};

namespace gctools {
std::string program_name();
std::string exe_name();
bool abort_flag(void);
}; // namespace gctools

// #endif // _clasp_memoryManagement_H

/*
    File: memoryManagement.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister

CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

See directory 'clasp/licenses' for full details.

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
