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
/* -^- */
#pragma once
//#ifndef _clasp_memoryManagement_H
//#define _clasp_memoryManagement_H

// Define compile-time flags that effect structure sizes
//
#include <atomic>
#include <clasp/gctools/configure_memory.h>
#include <clasp/gctools/hardErrors.h>

#ifdef USE_BOEHM
#ifdef CLASP_THREADS
  #define GC_THREADS
#endif
#include <gc/gc.h>
#include <gc/gc_allocator.h>
typedef void *LocationDependencyPtrT;
#endif // USE_BOEHM

#ifdef USE_MPS
extern "C" {
#include <clasp/mps/code/mps.h>
#include <clasp/mps/code/mpsavm.h>
};
typedef mps_ld_t LocationDependencyPtrT;
#endif

typedef int (*MainFunctionType)(int argc, char *argv[], bool &mpiEnabled, int &mpiRank, int &mpiSize);

#define GC_LOG(x)
#define GCPRIVATE public
#define GCPROTECTED public

#include <clasp/gctools/hardErrors.h>

namespace gctools {

template <typename T>
constexpr size_t depreciatedAlignmentT() { return alignof(T); };
template <typename T>
constexpr size_t depreciatedAlignUpT(size_t size) { return (size + depreciatedAlignmentT<T>() - 1) & ~(depreciatedAlignmentT<T>() - 1); };
};

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
template <class OT>
struct GCStamp;
extern size_t global_alignup_sizeof_header;
extern const char* _global_stack_marker;
extern size_t _global_stack_max_size;
};

#define STAMP_MTAG   BOOST_BINARY(00)
#define INVALID_MTAG        BOOST_BINARY(01)
#define FWD_MTAG            BOOST_BINARY(10)
#define STAMP_SHIFT  2
#define DO_SHIFT_STAMP(unshifted_stamp) ((unshifted_stamp<<STAMP_SHIFT)|STAMP_MTAG)
// ADJUST_STAMP values are left unshifted
#define ADJUST_STAMP(unshifted_stamp) (unshifted_stamp) // (unshifted_stamp<<STAMP_PARTIAL_SHIFT_REST_FIXNUM)|STAMP_MTAG)
// ISA_ADJUST_STAMP must be shifted so that they match header values when they are read
//     straight out of a header (they will already be shifted)
#define ISA_ADJUST_STAMP(unshifted_stamp) DO_SHIFT_STAMP(unshifted_stamp)
// TYPEQ_ADJUST_STAMP will be passed to make_fixnum - so it will be shifted
#define TYPEQ_ADJUST_STAMP(unshifted_stamp) (unshifted_stamp)


namespace gctools {

#ifdef USE_BOEHM
class Header_s;
#endif
#ifdef USE_MPS
class Header_s;
#endif

template <typename T>
struct GCHeader {
#ifdef USE_BOEHM
  typedef Header_s HeaderType;
#endif
#ifdef USE_MPS
#ifdef RUNNING_GC_BUILDER
  typedef Header_s HeaderType;
#else
  typedef Header_s HeaderType;
#endif
#endif
};

template <typename T>
struct GCAllocationPoint;
};

/*!
  Template struct:   DynamicCast

*/
#include <clasp/gctools/gc_boot.h>
#include <clasp/gctools/pointer_tagging.h>

namespace gctools {
    /*! This is the type of the tagged kind header that is the first
word of every object in memory managed by the GC */
  typedef uintptr_t stamp_t;
  typedef uintptr_t tagged_stamp_t;

};

namespace gctools {
  class GCObject {};
};

#include <clasp/gctools/threadlocal.fwd.h>

extern "C" {
const char *obj_name(gctools::stamp_t kind);
extern void obj_dump_base(void *base);
};



namespace gctools {
  extern int global_signalTrap;
  extern bool global_debuggerOnSIGABRT; // If this is false then SIGABRT is processed normally and it will lead to termination of the program. See core__exit!
#if 0
  void do_pollSignals();

#define SET_SIGNAL(s) { gctools::global_signalTrap = s; }
  inline void poll_signals() { if (gctools::global_signalTrap) gctools::do_pollSignals();};
#endif
};

namespace gctools {
/*! Allocate an atomic buffer with malloc */
char *clasp_alloc_atomic(size_t buffer_size);
/*! The buffer above must be deallocated using this call*/
void clasp_dealloc(char *buffer);
};


namespace gctools {


  
#define STAMP_DUMMY_FOR_CPOINTER 0
    typedef enum {
#if !defined(SCRAPING)
 #if defined(USE_BOEHM) || defined(RUNNING_GC_BUILDER)
  #define GC_ENUM
        STAMP_null = 0,
   #include INIT_CLASSES_INC_H // REPLACED CLASP_GC_FILENAME
  #undef GC_ENUM
 #endif
#endif        
#ifndef RUNNING_GC_BUILDER
 #ifdef USE_MPS
  #define GC_STAMP
   #include CLASP_GC_FILENAME
  #undef GC_STAMP
 #endif
#endif
        STAMP_VA_LIST_S = STAMP_core__VaList_dummy_O, 
        STAMP_CONS = STAMP_core__Cons_O, 
        STAMP_CHARACTER = STAMP_core__Character_dummy_O, 
        STAMP_UNUSED = STAMP_core__Unused_dummy_O, 
        STAMP_CPOINTER = STAMP_DUMMY_FOR_CPOINTER,
        STAMP_SINGLE_FLOAT = STAMP_core__SingleFloat_dummy_O, 
        STAMP_FIXNUM = STAMP_core__Fixnum_dummy_O,
        STAMP_INSTANCE = STAMP_core__Instance_O,
        STAMP_FUNCALLABLE_INSTANCE = STAMP_core__FuncallableInstance_O,
        STAMP_WRAPPED_POINTER = STAMP_core__WrappedPointer_O,
        STAMP_DERIVABLE = STAMP_core__DerivableCxxObject_O,
        STAMP_CLASS_REP = STAMP_clbind__ClassRep_O
    } GCStampEnum;

// These different positions represent tag tests in the dtree interpreter and
//   discriminating functions
#define FIXNUM_TEST       0x00
#define SINGLE_FLOAT_TEST 0x01
#define CHARACTER_TEST    0x02
#define CONS_TEST         0x03

};







namespace gctools {

   /*! Stamp - integer value that is written into the header in normal general objects 
               and into the Rack for Instance_O objects.
     See Header_s below for a description of the GC Header tag scheme.
     Stamp needs to fit within a Fixnum.
 */
  typedef uintptr_t UnshiftedStamp; // first 62 bits
  typedef uintptr_t ShiftedStamp; // High 62 bits
  extern std::atomic<UnshiftedStamp> global_NextUnshiftedStamp;

  template <class T>
    inline size_t sizeof_with_header();

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

                         stamp value   where-tag    mtag
      64 bits total -> |    60 bits |   2 bits  | 2 bits

      The 'mtag' - two least-significant bits of the header uintptr_t value describe
      what the rest of the header data means.  This is used for General_O and derived objects.
      #B00 == THIS MUST ALWAYS BE #B00 !!!!!!! EXCEPT MPS WILL CHANGE THEM FOR ITS PURPOSES.
              To be a valid Clasp object they must be #B00  The header MUST look like a FIXNUM
              This is the 'stamp_tag' and indicates that the other bits in the header
              represent a stamp value that indicate 
              whether there is an extended-stamp and where to find the extended-stamp.
      #B01 == (unused) This is an illegal value for the two lsbs,
              it indictates that this is not a valid header.
      #B10 == (MPS specific) FWD_MTAG - This tag indicates that the remaining data bits in the header contains a forwarding
              pointer.  The uintptr_t in additional_data[0] contains the length of
              the block from the client pointer.
      #B11 == (MPS specific) This indicates that the header contains a pad; check the
              bit at #B100 to see if the pad is a pad1 (==0) or a pad (==1)

      IMPORTANT!!!!!:
          The header values are designed to look like FIXNUMs - so we can read them
          out of the header and compare them to stamps without shifting or masking anything.
          This is confusing because if you read the header value #B10100 in Clasp
          and print it you will see 5 (five) but you expect 20 (twenty).
          This is because the value is treated as a FIXNUM within Clasp and 
          it looks like it's shifted >>2


      If the tag is a 'stamp_tag' then the data bits have this meaning...
                              stamp   where_tag
      64 bits total -> |    60 bits | 2 bits |
      
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

  class Header_s {
  public:
    static const tagged_stamp_t mtag_mask      =  BOOST_BINARY(0011);
      static const size_t mtag_shift = 2;
    static const tagged_stamp_t where_mask     =  BOOST_BINARY(1100);
// Must match the number of bits to describe where_mask from the 0th bit
    // This is the width of integer that llvm needs to represent the masked off part of a header stamp
    static const tagged_stamp_t where_tag_width=  4; 
    // These MUST match the wtags used in clasp-analyzer.lisp and scraper/code-generator.lisp
    static const tagged_stamp_t derivable_wtag =  BOOST_BINARY(0000);
    static const tagged_stamp_t rack_wtag      =  BOOST_BINARY(0100);
    static const tagged_stamp_t wrapped_wtag   =  BOOST_BINARY(1000);
    static const tagged_stamp_t header_wtag    =  BOOST_BINARY(1100);
    static const tagged_stamp_t wtag_shift     = 2;
    
    static const tagged_stamp_t invalid_tag=  INVALID_MTAG; // indicates not header
    // stamp_tag MUST be 00 so that stamps look like FIXNUMs
    static const tagged_stamp_t stamp_tag  =  STAMP_MTAG;
    static const tagged_stamp_t fwd_tag    =  FWD_MTAG;
    static const tagged_stamp_t pad_mask   = BOOST_BINARY(111);
    static const tagged_stamp_t pad_test   = BOOST_BINARY(011);
    static const int pad_shift = 3; // 3 bits for pad tag
    static const tagged_stamp_t pad_tag    = BOOST_BINARY(011);
    static const tagged_stamp_t pad1_tag   = BOOST_BINARY(111);
    static const tagged_stamp_t fwd_ptr_mask = ~tag_mask;
    static const tagged_stamp_t stamp_mask    = ~tag_mask; // BOOST_BINARY(11...11111111111100);
    static const int stamp_shift = STAMP_SHIFT;
    static const tagged_stamp_t largest_possible_stamp = stamp_mask>>stamp_shift;
  public:
    struct StampWtagMtag {
      tagged_stamp_t _value;
    StampWtagMtag() : _value(0) {};
      StampWtagMtag(UnshiftedStamp stamp) : _value(shift_unshifted_stamp(stamp)) {};
      // This is so we can find where we shift/unshift/don'tshift
      static UnshiftedStamp leave_unshifted_stamp(UnshiftedStamp us) {
        return (us);
      }
      static UnshiftedStamp first_NextUnshiftedStamp(UnshiftedStamp start) {
        return (start+(1<<stamp_shift))&(~mtag_mask);
      }
      static bool is_unshifted_stamp(uint64_t unknown) {
        // This is the only test that makes sense.
        if (is_header_stamp(unknown) && unknown <= STAMP_max) return true;
        // Otherwise it's an assigned stamp and it must be in the range below.
        if (STAMP_max<unknown && unknown<global_NextUnshiftedStamp) return true;
        return false;
      }
      static bool is_shifted_stamp(uint64_t unknown) {
        return !(unknown&mtag_mask); // Low two bits must be zero
      }
      static bool is_header_shifted_stamp(uint64_t unknown) {
        if ((unknown&mtag_mask)!=0) return false;
        uint64_t stamp = unshift_shifted_stamp(unknown);
        if ((unknown&where_mask)==header_wtag) {
          return (stamp<=STAMP_max);
        }
        if ((unknown&where_mask)==rack_wtag) {
          return (stamp == STAMP_core__Instance_O ||
                  stamp == STAMP_core__FuncallableInstance_O ||
                  stamp == STAMP_clbind__ClassRep_O);
        }
        if ((unknown&where_mask)==wrapped_wtag) {
          return (stamp == STAMP_core__WrappedPointer_O);
        }
        return (stamp == STAMP_core__DerivableCxxObject_O);
      }
      static bool is_rack_shifted_stamp(uint64_t unknown) {
        return ((unknown&mtag_mask)==0)&&((unknown&where_mask)==rack_wtag); // Low two bits must be zero
      }
      static bool is_wrapped_shifted_stamp(uint64_t unknown) {
        return ((unknown&mtag_mask)==0)&&((unknown&where_mask)==wrapped_wtag); // Low two bits must be zero
      }
      static bool is_derivable_shifted_stamp(uint64_t unknown) {
        return ((unknown&mtag_mask)==0)&&((unknown&where_mask)==derivable_wtag); // Low two bits must be zero
      }
      static bool is_header_stamp(uint64_t unknown) {
        return is_header_shifted_stamp(shift_unshifted_stamp(unknown));
      }
      static bool is_rack_stamp(uint64_t unknown) {
        return is_rack_shifted_stamp(shift_unshifted_stamp(unknown));
      }
      static bool is_wrapped_stamp(uint64_t unknown) {
        return is_wrapped_shifted_stamp(shift_unshifted_stamp(unknown));
      }
      static bool is_derivable_stamp(uint64_t unknown) {
        return is_derivable_shifted_stamp(shift_unshifted_stamp(unknown));
      }
      static ShiftedStamp shift_unshifted_stamp(UnshiftedStamp us) {
        return ((us<<Header_s::stamp_shift)|Header_s::stamp_tag);
      }
      static size_t make_nowhere_stamp(UnshiftedStamp us) {
        // Remove the where part of the unshifted stamp
        // The resulting value will be unique to the class and adjacent to each other
        //    andsuitable for indices into an array
        #ifdef DEBUG_ASSERT
        if (!is_unshifted_stamp(us)) {
          printf("%s:%d:%s the argument %lu must be an unshifted stamp\n", __FILE__, __LINE__, __FUNCTION__, us );
          abort();
        }
        #endif
        return (size_t)(us>>stamp_shift);
      }
      static size_t get_stamp_where(UnshiftedStamp us) {
        return (size_t)(us&(where_mask>>stamp_shift));
      }
      static UnshiftedStamp unshift_shifted_stamp(ShiftedStamp us) {
        return ((us>>Header_s::stamp_shift));
      }
      template <typename T>
      static StampWtagMtag make()
      {
        StampWtagMtag v(GCStamp<T>::Stamp);
        return v;
      }
      static StampWtagMtag make_instance()
      {
        StampWtagMtag v(STAMP_INSTANCE);
        return v;
      }
      static StampWtagMtag make_funcallable_instance()
      {
        StampWtagMtag v(STAMP_FUNCALLABLE_INSTANCE);
        return v;
      }
      static StampWtagMtag make_unknown(UnshiftedStamp the_stamp)
      {
        StampWtagMtag v(the_stamp);
        return v;
      }
    public:
      // GenerateHeaderValue must be passed to make_fixnum and the result exactly matches a header value
      template <typename T>
      static int64_t GenerateHeaderValue() { return (int64_t)GCStamp<T>::Stamp; };
    public: // header readers
      inline size_t mtag() const { return (size_t)(this->_value & mtag_mask);};
      inline bool pad1P() const { return (this->_value & pad_mask) == pad1_tag; };
      inline ShiftedStamp shifted_stamp() const {
        return static_cast<ShiftedStamp>( this->_value );
      }
      inline UnshiftedStamp unshifted_stamp() const {
        return static_cast<UnshiftedStamp>( unshift_shifted_stamp(this->_value) );
      }
      inline size_t nowhere_stamp() const {
        return make_nowhere_stamp(this->unshifted_stamp());
      }
    };
  public:
    static void signal_invalid_object(const Header_s* header, const char* msg);
  public:
    void validate() const;
    void quick_validate() const {
#ifdef DEBUG_QUICK_VALIDATE
      if ( this->stampP() ) {
#ifdef DEBUG_GUARD    
        if (this->guard != 0xFEEAFEEBDEADBEEF) signal_invalid_object(this,"bad head guard");
        if (this->_tail_size>0) {
          const unsigned char* tail = (const unsigned char*)this+this->_tail_start;
          if ((*tail) != 0xcc) signal_invalid_object(this,"bad tail not 0xcc");
        }
#endif
        if ( !is_unshifted_stamp(this->unshifted_stamp())) signal_invalid_object(this,"bad kind");
      }
#else
      this->validate();
#endif
    }
  public:
    // The header contains the stamp_wtag_mtag value.
    StampWtagMtag _stamp_wtag_mtag;
    // The additional_data[0] must fall right after the header or pads might try to write into the wrong place
    tagged_stamp_t additional_data[0]; // The 0th element intrudes into the client data unless DEBUG_GUARD is on
#ifdef DEBUG_GUARD
    int _tail_start;
    int _tail_size;
    tagged_stamp_t guard;
#endif
  public:
#if !defined(DEBUG_GUARD)
    
  Header_s(const StampWtagMtag& k) : _stamp_wtag_mtag(k) {}
#endif
#if defined(DEBUG_GUARD)
    inline void fill_tail() { memset((void*)(((char*)this)+this->_tail_start),0xcc,this->_tail_size);};
  Header_s(const StampWtagMtag& k,size_t tstart, size_t tsize, size_t total_size) 
    : _stamp_wtag_mtag(k),
      _tail_start(tstart),
      _tail_size(tsize),
      guard(0xFEEAFEEBDEADBEEF)
      {
        this->fill_tail();
      };
#endif
    static GCStampEnum value_to_stamp(Fixnum value) { return (GCStampEnum)(StampWtagMtag::unshift_shifted_stamp(value)); };
  public:
    size_t tag() const { return (size_t)(this->_stamp_wtag_mtag._value & tag_mask);};
#ifdef DEBUG_GUARD
    size_t tail_size() const { return this->_tail_size; };
#else
    constexpr size_t tail_size() const { return 0; };
#endif
    bool invalidP() const { return (this->_stamp_wtag_mtag._value & tag_mask) == invalid_tag; };
    bool stampP() const { return (this->_stamp_wtag_mtag._value & tag_mask) == stamp_tag; };
    bool fwdP() const { return (this->_stamp_wtag_mtag._value & tag_mask) == fwd_tag; };
    bool anyPadP() const { return (this->_stamp_wtag_mtag._value & pad_test) == pad_tag; };
    bool padP() const { return (this->_stamp_wtag_mtag._value & pad_mask) == pad_tag; };
    bool pad1P() const { return (this->_stamp_wtag_mtag._value & pad_mask) == pad1_tag; };
  /*! No sanity checking done - this function assumes kindP == true */
    ShiftedStamp shifted_stamp() const { return (ShiftedStamp)(this->_stamp_wtag_mtag._value); };
      GCStampEnum stamp_wtag() const { return (GCStampEnum)(value_to_stamp(this->_stamp_wtag_mtag._value)); };
      GCStampEnum stamp_() const { return (GCStampEnum)(value_to_stamp(this->_stamp_wtag_mtag._value)>>wtag_shift); };
  /*! No sanity checking done - this function assumes fwdP == true */
    void *fwdPointer() const { return reinterpret_cast<void *>(this->_stamp_wtag_mtag._value & fwd_ptr_mask); };
  /*! Return the size of the fwd block - without the header. This reaches into the client area to get the size */
    void setFwdPointer(void *ptr) { this->_stamp_wtag_mtag._value = reinterpret_cast<tagged_stamp_t>(ptr) | fwd_tag; };
    tagged_stamp_t fwdSize() const { return this->additional_data[0]; };
  /*! This writes into the first tagged_stamp_t sized word of the client data. */
    void setFwdSize(size_t sz) { this->additional_data[0] = sz; };
  /*! Define the header as a pad, pass pad_tag or pad1_tag */
    void setPad(tagged_stamp_t p) { this->_stamp_wtag_mtag._value = p; };
  /*! Return the pad1 size */
    tagged_stamp_t pad1Size() const { return alignof(Header_s); };
  /*! Return the size of the pad block - without the header */
    tagged_stamp_t padSize() const { return (this->additional_data[0]); };
  /*! This writes into the first tagged_stamp_t sized word of the client data. */
    void setPadSize(size_t sz) { this->additional_data[0] = sz; };
  /*! Write the stamp to the stamp bits */
    string description() const {
      if (this->stampP()) {
        std::stringstream ss;
        ss << "Header=" << (void *)(this->_stamp_wtag_mtag._value);
        ss << "/";
        ss << obj_name(this->stamp_());
        return ss.str();
      } else if (this->fwdP()) {
        std::stringstream ss;
        ss << "Fwd/ptr=" << this->fwdPointer() << "/sz=" << this->fwdSize();
        return ss.str();
      } else if (this->pad1P()) {
        return "Pad1";
      } else if (this->padP()) {
        stringstream ss;
        ss << "Pad/sz=" << this->padSize();
        return ss.str();
      }
      stringstream ss;
      ss << "IllegalHeader=";
      ss << (void *)(this->_stamp_wtag_mtag._value);
      printf("%s:%d Header->description() found an illegal header = %s\n", __FILE__, __LINE__, ss.str().c_str());
      return ss.str();
      ;
    }
  };
};

// ------------------------------------------------------------
//
// Stamp
//

namespace gctools {
  /* NextUnshiftedStamp(...) returns a unique Stamp value every time it is called.
     They are generated when creating and redefining classes and
     must be unique system-wide.  They are used for generic function dispatch.
  */

  /*! global_NextBuiltInStamp starts at STAMP_max+1
      See definition in memoryManagement.cc
      This is so that it doesn't use any stamps that were set by the static analyzer. */
  extern std::atomic<UnshiftedStamp> global_NextUnshiftedStamp;
  /*! Return a new stamp for BuiltIn classes.
      If given != STAMP_null then simply return give as the stamp.
      Otherwise return the global_NextBuiltInStamp and advance it
      to the next one */
  void OutOfStamps();
inline ShiftedStamp NextStampWtag(ShiftedStamp where, UnshiftedStamp given = STAMP_null) {
    if ( given != STAMP_null ) {
      return Header_s::StampWtagMtag::shift_unshifted_stamp(given)|where;
    }
    if (global_NextUnshiftedStamp.load() < Header_s::largest_possible_stamp) {
      UnshiftedStamp stamp = global_NextUnshiftedStamp.fetch_add(4);
#ifdef DEBUG_ASSERT
      if (!(Header_s::StampWtagMtag::is_unshifted_stamp(stamp)) && (stamp&3)!=0) {
        printf("%s:%d NextStampWtag is about to return a stamp that is illegal: stamp: %lu\n", __FILE__, __LINE__, stamp);
      }
#endif
      return Header_s::StampWtagMtag::shift_unshifted_stamp(stamp)|where;
    }
    OutOfStamps();
    abort();
  }
};





#ifdef USE_BOEHM
#include <clasp/gctools/boehmGarbageCollection.h>
#endif

#ifdef USE_MPS
#include <clasp/gctools/mpsGarbageCollection.h>
#endif


namespace core {
  class BuiltinClosure_O;
};


#include <clasp/gctools/cast.h>
#include <clasp/gctools/tagged_cast.h>

namespace gctools {

  constexpr size_t Alignment() {
//  return sizeof(Header_s);
    return alignof(Header_s);
  };
  inline constexpr size_t AlignUp(size_t size) { return (size + Alignment() - 1) & ~(Alignment() - 1); };

  // ----------------------------------------------------------------------
  //! Calculate the size of an object + header for allocation
  template <class T>
    inline size_t sizeof_with_header() { return AlignUp(sizeof(T)) + sizeof(Header_s); }


/*! Size of containers given the number of elements */
  template <typename Cont_impl>
    size_t sizeof_container(size_t n) {
    size_t classSz = sizeof(Cont_impl);
    size_t dataSz = sizeof(typename Cont_impl::value_type) * n;
    size_t totalSz = classSz + dataSz;
    return AlignUp(totalSz);
  };

  template <class T>
    inline size_t sizeof_container_with_header(size_t num) {
    return sizeof_container<T>(num) + sizeof(Header_s);
  };

  /*! Size of containers given the number of binits where BinitWidth is the number of bits/bunit */
  template <typename Cont_impl>
    size_t sizeof_bitunit_container(size_t n) {
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

  template <class T>
    inline size_t sizeof_bitunit_container_with_header(size_t num) {
    size_t size_bitunit_container = sizeof_bitunit_container<T>(num);
    size_t size_header = sizeof(Header_s);
    size_t sum = size_bitunit_container+size_header;
#ifdef DEBUG_BITUNIT_CONTAINER
    printf("%s:%d  In sizeof_bitunit_container_with_header  num = %lu\n", __FILE__, __LINE__, num);
    printf("%s:%d  In sizeof_bitunit_container_with_header   size_bitunit_container = %lu\n", __FILE__, __LINE__, size_bitunit_container);
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
  inline size_t Align(size_t size) {
    return ((AlignUp(size) >= global_sizeof_fwd) ? AlignUp(size) : global_sizeof_fwd);
  };
};


namespace gctools {

#ifdef DEBUG_ENSURE_VALID_OBJECT
#define EXHAUSTIVE_VALIDATE(ptr) (ptr)->quick_validate();
#else
#define EXHAUSTIVE_VALIDATE(ptr)
#endif
  
  inline const void *ClientPtrToBasePtr(const void *mostDerived) {
    const void *ptr = reinterpret_cast<const char *>(mostDerived) - sizeof(Header_s);
    return ptr;
  }

  inline void *ClientPtrToBasePtr(void *mostDerived) {
    void *ptr = reinterpret_cast<char *>(mostDerived) - sizeof(Header_s);
    return ptr;
  }

  inline const Header_s* header_pointer(const void* client_pointer)
  {
    const Header_s* header = reinterpret_cast<const Header_s*>(reinterpret_cast<const char*>(client_pointer) - sizeof(Header_s));
    return header;
  }
  
  inline void throwIfInvalidClient(core::T_O *client) {
    Header_s *header = (Header_s *)ClientPtrToBasePtr(client);
    if (header->invalidP()) {
      throw_hard_error_bad_client((void*)client);
    }
  }

  template <typename T>
    inline T *BasePtrToMostDerivedPtr(void *base) {
    T *ptr = reinterpret_cast<T *>(reinterpret_cast<char *>(base) + sizeof(Header_s));
    return ptr;
  }
};


extern "C" {
const char *obj_name(gctools::stamp_t kind);
const char *obj_kind_name(core::T_O *ptr);
size_t obj_kind(core::T_O *ptr);
extern void obj_dump_base(void *base);
};

namespace gctools {
/*! Specialize GcKindSelector so that it returns the appropriate GcKindEnum for OT */
  template <class OT>
    struct GCStamp {
#ifdef USE_MPS
#ifdef RUNNING_GC_BUILDER
      static GCStampEnum const Stamp = STAMP_null;
#else
  // We need a default Kind when running the gc-builder.lsp static analyzer
  // but we don't want a default Kind when compiling the mps version of the code
  // to force compiler errors when the Kind for an object hasn't been declared
      static GCStampEnum const Stamp = STAMP_null; // provide default for weak dependents
#endif // RUNNING_GC_BUILDER
#endif // USE_MPS
#ifdef USE_BOEHM
#ifdef USE_CXX_DYNAMIC_CAST
      static GCStampEnum const Stamp = STAMP_null; // minimally define STAMP_null
#else
                                            // We don't want a default Kind when compiling the boehm version of the code
                                            // to force compiler errors when the Kind for an object hasn't been declared
#endif // USE_CXX_DYNAMIC_CAST
#endif
    };
};


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
  class VaList_dummy_O;
  class Instance_O;
  class FuncallableInstance_O;
}


namespace gctools {

/*
 * atomic == Object contains no internal tagged pointers, is collectable
 * normal == Object contains internal tagged pointers, is collectable
 * collectable_immobile == Object cannot be moved but is collectable
 * unmanaged == Object cannot be moved and cannot be automatically collected
 */
  typedef enum { atomic,
                 normal,
                 collectable_immobile,
                 unmanaged } GCInfo_policy;
  
template <class OT>
struct GCInfo {
  static bool const NeedsInitialization = true; // Currently, by default,  everything needs initialization
  static bool const NeedsFinalization = false;  // By default, nothing needs finalization
  static constexpr GCInfo_policy Policy = normal;
};
};

#include <clasp/gctools/smart_pointers.h>

#include <clasp/core/coretypes.h>


namespace gctools {
  extern int global_pollTicksPerCleanup;

template <typename T>
void *SmartPtrToBasePtr(smart_ptr<T> obj) {
  void *ptr;
  if (obj.objectp()) {
    ptr = reinterpret_cast<void *>(reinterpret_cast<char *>(obj.untag_object()) - sizeof(Header_s));
  } else {
    throw_hard_error("Bad pointer for SmartPtrToBasePtr");
  }
  return ptr;
}
};


namespace core {
  class ThreadLocalState;
};

/*! Declare this in the top namespace */
extern THREAD_LOCAL core::ThreadLocalState *my_thread;


#include <clasp/gctools/gcStack.h>
//#include <clasp/gctools/gcalloc.h>

#define GC_ALLOCATE(_class_, _obj_) gctools::smart_ptr<_class_> _obj_ = gctools::GC<_class_>::allocate_with_default_constructor()
#define GC_ALLOCATE_VARIADIC(_class_, _obj_, ...) gctools::smart_ptr<_class_> _obj_ = gctools::GC<_class_>::allocate(__VA_ARGS__)
#define GC_ALLOCATE_UNCOLLECTABLE(_class_, _obj_, ...) gctools::smart_ptr<_class_> _obj_ = gctools::GC<_class_>::root_allocate(__VA_ARGS__)

#define GC_COPY(_class_, _obj_, _orig_) gctools::smart_ptr<_class_> _obj_ = gctools::GC<_class_>::copy(_orig_)

/*! These don't do anything at the moment
  but may be used in the future to create unsafe-gc points
*/

#define SUPPRESS_GC()  {}
#define ENABLE_GC() {}

namespace gctools {

int handleFatalCondition();

 
/* Start up the garbage collector and the main function.
       The main function is wrapped within this function */
int startupGarbageCollectorAndSystem(MainFunctionType startupFn, int argc, char *argv[], size_t stackMax, bool mpiEnabled, int mpiRank, int mpiSize);
};


namespace gctools {
  void rawHeaderDescribe(const uintptr_t *headerP);
};

extern "C" {
// These must be provided the the garbage collector specific code


//! Describe the header of the client
void client_describe(void *taggedClient);
//! Validate the client
void client_validate_tagged(gctools::Tagged taggedClient);
//! Must be a General_O ptr - no tag
void client_validate_General_O_ptr(const core::General_O* client_ptr);
//! Validate a client smart_ptr - only general objects
void client_validate(core::T_sp client);
//! Describe the header
void header_describe(gctools::Header_s* headerP);
};

//#include <clasp/gctools/containers.h>


namespace gctools {
#define LITERAL_TAG_CHAR 0
#define TRANSIENT_TAG_CHAR 1

void untag_literal_index(size_t findex, size_t& index, size_t& tag);
};



namespace gctools {

  /*! Maintains pointers to arrays of roots that are stored in LLVM Modules
      we add and remove during runtime as Modules are compiled and (in the future) removed.
      MPS and Boehm have different needs to keep track of roots.
      MPS just needs one pointer to memory in the module and
      Boehm needs to maintain a shadow copy in the Boehm managed memory because
        I wasn't able to get GC_add_roots to work properly.
        TODO: get GC_add_roots to work
   */


  struct GCRootsInModule {
    static int const TransientRawIndex = 0;
    static size_t const DefaultCapacity = 256;
    // Fields
    core::SimpleVector_O** _TransientAlloca;
    void* _boehm_shadow_memory;
    void* _module_memory;
    size_t _num_entries;
    size_t _capacity;
    /*fnLispCallingConvention* */ void** _function_pointers;
    void** _function_descriptions;
    size_t _function_pointer_count;
    GCRootsInModule(void* shadow_mem, void* module_mem, size_t num_entries, core::SimpleVector_O** transient_alloca, size_t transient_entries, size_t function_pointer_count, void** fptrs, void** fdescs);
    GCRootsInModule(size_t capacity = DefaultCapacity);
    void setup_transients(core::SimpleVector_O** transient_alloca, size_t transient_entries);
    
    size_t remainingCapacity() { return this->_capacity - this->_num_entries;};
    size_t push_back(Tagged val);
    Tagged setLiteral(size_t index, Tagged val);
    Tagged getLiteral(size_t index);
    Tagged setTransient(size_t index, Tagged val);
    Tagged getTransient(size_t index);
    Tagged setTaggedIndex(char tag, size_t index, Tagged val);
    Tagged getTaggedIndex(char tag, size_t index);
    /*fnLispCallingConvention*/ void* lookup_function(size_t index);
    void* lookup_function_description(size_t index);
    void* address(size_t index) {
      return reinterpret_cast<void*>(&reinterpret_cast<core::T_sp*>(this->_module_memory)[index+1]);
    }
  };

  extern std::atomic<uint64_t> global_NumberOfRootTables;
  extern std::atomic<uint64_t> global_TotalRootTableSize;
  
void initialize_gcroots_in_module(GCRootsInModule* gcroots_in_module, core::T_O** root_address, size_t num_roots, gctools::Tagged initial_data, core::SimpleVector_O** transientAlloca, size_t transient_entries, size_t function_pointer_number, void** fptrs, void** fdescs);
  core::T_O* read_gcroots_in_module(GCRootsInModule* roots, size_t index);
  void shutdown_gcroots_in_module(GCRootsInModule* gcroots_in_module);

  inline core::T_O* ensure_valid_object(core::T_O* tagged_object) {
#ifdef DEBUG_ENSURE_VALID_OBJECT
  // Only validate general objects for now
    if (tagged_generalp(tagged_object)) {
      core::T_O* untagged_object = gc::untag_general(tagged_object);
      Header_s* header = reinterpret_cast<Header_s*>(ClientPtrToBasePtr(untagged_object));
      header->quick_validate();
    }
#endif
    return tagged_object;
  }
  template <typename OT>
    inline gctools::smart_ptr<OT> ensure_valid_object(gctools::smart_ptr<OT> tagged_object) {
#ifdef DEBUG_ENSURE_VALID_OBJECT
  // Only validate general objects for now
    if (tagged_generalp(tagged_object.raw_())) {
      core::T_O* untagged_object = gc::untag_general(tagged_object.raw_());
      Header_s* header = reinterpret_cast<Header_s*>(ClientPtrToBasePtr(untagged_object));
      header->quick_validate();
    }
#endif
    return tagged_object;
  }
};


extern "C" {
// Invoke mps_park/mps_release or boehm_park/boehm_release
void gc_park();
void gc_release();
};

#ifdef DEBUG_ENSURE_VALID_OBJECT
#define ENSURE_VALID_OBJECT(x) (gctools::ensure_valid_object(x))
#define EVO(x) (gctools::ensure_valid_object(x))
#else
#define ENSURE_VALID_OBJECT(x) x
#define EVO(x)
#endif

namespace gctools {
struct SafeGCPark {
  SafeGCPark() {
    gc_park();
  };
  ~SafeGCPark() {
    gc_release();
  }
};
};

//#endif // _clasp_memoryManagement_H
