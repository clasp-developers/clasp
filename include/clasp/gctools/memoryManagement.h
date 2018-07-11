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
  typedef uintptr_clasp_t stamp_t;
  typedef uintptr_clasp_t tagged_stamp_t;

};

namespace gctools {
  class GCObject {};
};

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
        STAMP_CPOINTER = STAMP_DUMMY_FOR_CPOINTER,
        STAMP_SINGLE_FLOAT = STAMP_core__SingleFloat_dummy_O, 
        STAMP_FIXNUM = STAMP_core__Fixnum_dummy_O,
        STAMP_INSTANCE = STAMP_core__Instance_O,
        STAMP_FUNCALLABLE_INSTANCE = STAMP_core__FuncallableInstance_O,
        STAMP_WRAPPED_POINTER = STAMP_core__WrappedPointer_O
    } GCStampEnum;

};







namespace gctools {

   /*! Stamp - integer value that is written into the header in normal general objects 
               and into the Rack for Instance_O objects.
     See Header_s below for a description of the GC Header tag scheme.
     Stamp needs to fit within a Fixnum.
 */
  typedef uintptr_clasp_t Stamp;
  extern std::atomic<Stamp> global_NextStamp;

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
      A header is 8 bytes long and consists of one uintptr_clasp_t (8 bytes) value.
      The structure of the header is...

      The (header) uintptr_clasp_t is a tagged value where the
      two least significant bits are the tag.

                              data       tag
      64 bits total -> |    62 bits | 2 bits |

      The 'tag' - two least-significant bits of the header uintptr_clasp_t value describe
      what the rest of the header data means.  This is used for General_O and derived objects.
      #B00 == This is an illegal value for the two lsbs,
              it indictates that this is not a valid header.
      #B01 == This is the 'stamp_tag' and indicates that the other bits in the header
              represent a stamp value that indicate 
              whether there is an extended-stamp and where to find the extended-stamp.
      #B10 == This tag indicates that the remaining data bits in the header contains a forwarding
              pointer.  The uintptr_clasp_t in additional_data[0] contains the length of
              the block from the client pointer.
      #B11 == This indicates that the header contains a pad; check the
              bit at #B100 to see if the pad is a pad1 (==0) or a pad (==1)

      If the tag is a 'stamp_tag' then the data bits have this meaning...
                              stamp      tag
      64 bits total -> |    62 bits | 2 bits |
      
      The 'stamp' is a 62 bit value (zero extended, 0==illegal) that tells
      the MPS GC what the layout of the object is and is used to determine
      IsA relationships between classes.

      The STAMP is also used by the fastgf generic function dispatch method.
      Exposed C++ classes that inherit from Instance_O store an extended-stamp in the rack.
      The header stamp is used to tell the system when it needs to look elsewhere for the extended-stamp.
      See cc_read_stamp in fastgf.cc for details
      If an object has an extended-stamp then the extended-stamp is used for fastgf generic function
      dispatch and stamp is used by MPS to determine the object layout and C++ IsA relationships.
      Each time a standard-class is redefined a new STAMP is generated and that 
      is later written into the Instance_O rack.

      The header ends with a uintptr_clasp_t additional_data[0], an array of uintptr_clasp_t which intrudes
      into the client data and is used when some header tags (fwd, pad) need it.  
      NOTE!!!   Writing anything into header.additional_data[0] wipes out the objects vtable and completely
                invalidates it - this is only used by the MPS GC when it's ok to invalidate the object.
    */

  class Header_s {
  public:
    static const tagged_stamp_t tag_mask   =  BOOST_BINARY(11);
    static const tagged_stamp_t invalid_tag=  BOOST_BINARY(00); // indicates not header
    static const tagged_stamp_t stamp_tag  =  BOOST_BINARY(01); // KIND = tagged_value>>2
    static const tagged_stamp_t fwd_tag    =  BOOST_BINARY(10);
    static const tagged_stamp_t pad_mask   = BOOST_BINARY(111);
    static const tagged_stamp_t pad_test   = BOOST_BINARY(011);
    static const int pad_shift = 3; // 3 bits for pad tag
    static const tagged_stamp_t pad_tag    = BOOST_BINARY(011);
    static const tagged_stamp_t pad1_tag   = BOOST_BINARY(111);
    static const tagged_stamp_t fwd_ptr_mask = ~tag_mask;
    static const tagged_stamp_t stamp_mask    = ~tag_mask; // BOOST_BINARY(11...11111111111100);
    static const int stamp_shift = 2;
    static const tagged_stamp_t largest_possible_stamp = stamp_mask>>stamp_shift;
  public:
    struct Value {
      tagged_stamp_t _value;
    Value() : _value(0) {};
    Value(GCStampEnum stamp) : _value((stamp << stamp_shift) | stamp_tag) {};
      template <typename T>
      static Value make()
      {
        Value v(GCStamp<T>::Stamp);
        return v;
      }
      static Value make_instance()
      {
        Value v(STAMP_INSTANCE);
        return v;
      }
      static Value make_funcallable_instance()
      {
        Value v(STAMP_FUNCALLABLE_INSTANCE);
        return v;
      }
      static Value make_unknown(GCStampEnum the_stamp)
      {
        Value v(the_stamp);
        return v;
      }
    public:
      template <typename T>
      static size_t GenerateHeaderValue() { return (GCStamp<T>::Stamp<<stamp_shift)|stamp_tag; };
    public: // header readers
      inline size_t tag() const { return (size_t)(this->_value & tag_mask);};
      inline bool pad1P() const { return (this->_value & pad_mask) == pad1_tag; };
      inline GCStampEnum stamp() const {
        return static_cast<GCStampEnum>( this->_value >> stamp_shift );
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
        if ( this->stamp() > global_NextStamp ) signal_invalid_object(this,"bad kind");
      }
#else
      this->validate();
#endif
    }
  public:
    Value header;
    // The additional_data[0] must fall right after the header or pads might try to write into the wrong place
    tagged_stamp_t additional_data[0]; // The 0th element intrudes into the client data unless DEBUG_GUARD is on
#ifdef DEBUG_GUARD
    int _tail_start;
    int _tail_size;
    tagged_stamp_t guard;
#endif
  public:
#if !defined(DEBUG_GUARD) 
  Header_s(const Value& k) : header(k) {}
#endif
#if defined(DEBUG_GUARD)
    inline void fill_tail() { memset((void*)(((char*)this)+this->_tail_start),0xcc,this->_tail_size);};
  Header_s(const Value& k,size_t tstart, size_t tsize, size_t total_size) 
    : header(k),
      _tail_start(tstart),
      _tail_size(tsize),
      guard(0xFEEAFEEBDEADBEEF)
      {
        this->fill_tail();
      };
#endif
    static GCStampEnum value_to_stamp(Fixnum value) { return (GCStampEnum)((value&stamp_mask) >> stamp_shift); };
  public:
    size_t tag() const { return (size_t)(this->header._value & tag_mask);};
#ifdef DEBUG_GUARD
    size_t tail_size() const { return this->_tail_size; };
#else
    constexpr size_t tail_size() const { return 0; };
#endif
    bool invalidP() const { return (this->header._value & tag_mask) == invalid_tag; };
    bool stampP() const { return (this->header._value & tag_mask) == stamp_tag; };
    bool fwdP() const { return (this->header._value & tag_mask) == fwd_tag; };
    bool anyPadP() const { return (this->header._value & pad_test) == pad_tag; };
    bool padP() const { return (this->header._value & pad_mask) == pad_tag; };
    bool pad1P() const { return (this->header._value & pad_mask) == pad1_tag; };
  /*! No sanity checking done - this function assumes kindP == true */
    GCStampEnum stamp() const { return (GCStampEnum)(value_to_stamp(this->header._value)); };
  /*! setKind wipes out the stamp */
//      void setKind(GCStampEnum k) { this->header._value = (k << stamp_shift) | stamp_tag; };
  /*! No sanity checking done - this function assumes fwdP == true */
    void *fwdPointer() const { return reinterpret_cast<void *>(this->header._value & fwd_ptr_mask); };
  /*! Return the size of the fwd block - without the header. This reaches into the client area to get the size */
    void setFwdPointer(void *ptr) { this->header._value = reinterpret_cast<tagged_stamp_t>(ptr) | fwd_tag; };
    tagged_stamp_t fwdSize() const { return this->additional_data[0]; };
  /*! This writes into the first tagged_stamp_t sized word of the client data. */
    void setFwdSize(size_t sz) { this->additional_data[0] = sz; };
  /*! Define the header as a pad, pass pad_tag or pad1_tag */
    void setPad(tagged_stamp_t p) { this->header._value = p; };
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
        ss << "Header=" << (void *)(this->header._value);
        ss << "/";
        ss << obj_name(this->stamp());
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
      ss << (void *)(this->header._value);
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
  /* NextStamp(...) returns a unique Stamp value every time it is called.
     They are generated when creating and redefining classes and
     must be unique system-wide.  They are used for generic function dispatch.
  */

  /*! global_NextBuiltInStamp starts at STAMP_max+1
      See definition in memoryManagement.cc
      This is so that it doesn't use any stamps that were set by the static analyzer. */
  extern std::atomic<Stamp> global_NextStamp;
  /*! Return a new stamp for BuiltIn classes.
      If given != STAMP_null then simply return give as the stamp.
      Otherwise return the global_NextBuiltInStamp and advance it
      to the next one */
  void OutOfStamps();
  inline Stamp NextStamp(Stamp given = STAMP_null) {
    if ( given != STAMP_null ) return given;
    if (global_NextStamp.load() < Header_s::largest_possible_stamp) {
      return global_NextStamp.fetch_add(1);
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


namespace gctools {

  struct MonitorAllocations {
    bool on;
    bool stackDump;
    int counter;
    int start;
    int end;
    int backtraceDepth;
  MonitorAllocations() : on(false), stackDump(false), counter(0){};
  };
  extern MonitorAllocations global_monitorAllocations;

  extern void monitorAllocation(stamp_t k, size_t sz);
  extern uint64_t globalBytesAllocated;

#if defined(TRACK_ALLOCATIONS) && defined(DEBUG_SLOW)
  inline void monitor_allocation(const stamp_t k, size_t sz) {
    globalBytesAllocated += sz;
#ifdef GC_MONITOR_ALLOCATIONS
    if ( global_monitorAllocations.on ) {
      monitorAllocation(k,sz);
    }
#endif
  }
#else
  inline void monitor_allocation(const stamp_t k, size_t sz) {};
#endif

};

extern "C" {
const char *obj_name(gctools::stamp_t kind);
const char *obj_kind_name(core::T_O *ptr);
size_t obj_kind(core::T_O *ptr);
extern void obj_dump_base(void *base);
};

extern "C" void HitAllocationSizeThreshold();
extern "C" void HitAllocationNumberThreshold();

namespace gctools {
  struct GlobalAllocationProfiler {
    std::atomic<int64_t> _AllocationNumberCounter;
    std::atomic<int64_t> _AllocationSizeCounter;
    std::atomic<int64_t> _HitAllocationNumberCounter;
    std::atomic<int64_t> _HitAllocationSizeCounter;
    size_t               _AllocationNumberThreshold;
    size_t               _AllocationSizeThreshold;
  GlobalAllocationProfiler(size_t size, size_t number) : _AllocationNumberThreshold(number), _AllocationSizeThreshold(size) {};
    
    inline void registerAllocation(size_t size) {
#ifdef DEBUG_MEMORY_PROFILE
      this->_AllocationSizeCounter += size;
      if (this->_AllocationSizeCounter >= this->_AllocationSizeThreshold) {
        this->_AllocationSizeCounter -= this->_AllocationSizeThreshold;
        HitAllocationSizeThreshold();
      }
      this->_AllocationNumberCounter++;
      if (this->_AllocationNumberCounter >= this->_AllocationNumberThreshold) {
        this->_AllocationNumberCounter = 0;
        HitAllocationNumberThreshold();
      }
#endif
  };
  };
};

extern gctools::GlobalAllocationProfiler global_AllocationProfiler;


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
namespace gctools {
  void lisp_disable_interrupts(core::ThreadLocalState* t);
  void lisp_enable_interrupts(core::ThreadLocalState* t);
  void lisp_check_pending_interrupts(core::ThreadLocalState* thread);
  void lisp_increment_recursive_allocation_counter(core::ThreadLocalState* thread);
  void lisp_decrement_recursive_allocation_counter(core::ThreadLocalState* thread);
};


namespace core {
  struct RAIIDisableInterrupts {
    ThreadLocalState* this_thread;
  RAIIDisableInterrupts(ThreadLocalState* t) : this_thread(t) {
    gctools::lisp_disable_interrupts(this->this_thread);
  }
    ~RAIIDisableInterrupts() {
      gctools::lisp_enable_interrupts(this->this_thread);
    }
  };
};


/*! Declare this in the top namespace */
extern THREAD_LOCAL core::ThreadLocalState *my_thread;
#define RAII_DISABLE_INTERRUPTS() core::RAIIDisableInterrupts disable_interrupts__(my_thread)

namespace core {
  #ifdef DEBUG_RECURSIVE_ALLOCATIONS
struct RecursiveAllocationCounter {
  RecursiveAllocationCounter() {
    gctools::lisp_increment_recursive_allocation_counter(my_thread);
  };
  ~RecursiveAllocationCounter() {
    gctools::lisp_decrement_recursive_allocation_counter(my_thread);
  }
};
#endif
#ifdef DEBUG_RECURSIVE_ALLOCATIONS
#define DO_DEBUG_RECURSIVE_ALLOCATIONS() ::core::RecursiveAllocationCounter _rac_;
#else
#define DO_DEBUG_RECURSIVE_ALLOCATIONS()
#endif
};


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
  void rawHeaderDescribe(const uintptr_clasp_t *headerP);
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

  /*! Maintains pointers to arrays of roots that are stored in LLVM Modules
      we add and remove during runtime as Modules are compiled and (in the future) removed.
      MPS and Boehm have different needs to keep track of roots.
      MPS just needs one pointer to memory in the module and
      Boehm needs to maintain a shadow copy in the Boehm managed memory because
        I wasn't able to get GC_add_roots to work properly.
        TODO: get GC_add_roots to work
   */
  struct GCRootsInModule {
    static size_t const DefaultCapacity = 256;
    void* _boehm_shadow_memory;
    void* _module_memory;
    size_t _num_entries;
    size_t _capacity;

    GCRootsInModule(void* shadow_mem, void* module_mem, size_t num_entries) {
      this->_boehm_shadow_memory = shadow_mem;
      this->_module_memory = module_mem;
      this->_num_entries = num_entries;
      this->_capacity = num_entries;
    }
    GCRootsInModule(size_t capacity = DefaultCapacity) {
#ifdef USE_BOEHM
      core::T_O** shadow_mem = reinterpret_cast<core::T_O**>(boehm_create_shadow_table(capacity));
      core::T_O** module_mem = shadow_mem;
#endif
#ifdef USE_MPS
      core::T_O** shadow_mem = reinterpret_cast<core::T_O**>(NULL);
      core::T_O** module_mem = reinterpret_cast<core::T_O**>(malloc(sizeof(core::T_O*)*capacity));
#endif
      this->_boehm_shadow_memory = shadow_mem;
      this->_module_memory = module_mem;
      this->_num_entries = 0;
      this->_capacity = capacity;
      memset(module_mem, 0, sizeof(core::T_O*)*capacity);
#ifdef USE_MPS
  // MPS registers the roots with the GC and doesn't need a shadow table
      mps_register_roots(reinterpret_cast<void*>(module_mem),capacity);
#endif
    }
    size_t remainingCapacity() { return this->_capacity - this->_num_entries;};
    size_t push_back(Tagged val);
    Tagged set(size_t index, Tagged val);
    Tagged get(size_t index);
    void* address(size_t index) {
      return reinterpret_cast<void*>(&reinterpret_cast<core::T_sp*>(this->_module_memory)[index]);
    }

  };
  
  void initialize_gcroots_in_module(GCRootsInModule* gcroots_in_module, core::T_O** root_address, size_t num_roots, gctools::Tagged initial_data);
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

#ifdef DEBUG_ENSURE_VALID_OBJECT
#define ENSURE_VALID_OBJECT(x) (gctools::ensure_valid_object(x))
#define EVO(x) (gctools::ensure_valid_object(x))
#else
#define ENSURE_VALID_OBJECT(x) x
#define EVO(x)
#endif

//#endif // _clasp_memoryManagement_H
