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
#include <clasp/core/foundation.h>

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
struct GCKind;
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
  typedef uintptr_t kind_t;
  typedef uintptr_t tagged_kind_t;

};

namespace gctools {
  class GCObject {};
};

extern "C" {
const char *obj_name(gctools::kind_t kind);
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
/*! GCKindEnum has one integer value for each type allocated by the GC.
This value is written into the Header_s of every allocated object.
Immediate (FIXNUM, SINGLE-FLOAT, CHARACTER)  and CONS have KIND values reserved.
If USE_CXX_DYNAMIC_CAST is defined then GCKindEnum has only one value 
and every header contains that KIND value (KIND_null)
and C++ dynamic_cast<...> is used to determine IsA relationships.
If USE_CXX_DYNAMIC_CAST is not defined then the GCKindEnum values calculated by
the clasp-analyzer static analyzer they are used along with template functions that
calculate IsA relationships using simple GCKindEnum range comparisons.
*/

#if defined(USE_CXX_DYNAMIC_CAST) || defined(RUNNING_GC_BUILDER)
  typedef enum { KIND_null = 0,
                 KIND_FIXNUM = 1,
                 KIND_SINGLE_FLOAT = 2,
                 KIND_CHARACTER = 3,
                 KIND_CONS = 4,
                 KIND_VA_LIST_S = 5,
                 KIND_INSTANCE = 6,
                 // These are defined to support the GCKind<...> specializations below
                 // when defined(USE_CXX_DYNAMIC_CAST) || defined(RUNNING_GC_BUILDER)
                 KIND_LISPALLOC_core__VaList_dummy_O = KIND_VA_LIST_S, 
                 KIND_LISPALLOC_core__Cons_O = KIND_CONS, 
                 KIND_LISPALLOC_core__Character_dummy_O = KIND_CHARACTER, 
                 KIND_LISPALLOC_core__SingleFloat_dummy_O = KIND_SINGLE_FLOAT, 
                 KIND_LISPALLOC_core__Fixnum_dummy_O = KIND_FIXNUM,
                 KIND_LISPALLOC_core__Instance_O = KIND_INSTANCE,
                 
                 KIND_max = 6 } GCKindEnum; // minimally define this GCKind
#else
 #define GC_ENUM
    typedef enum {
 #include CLASP_GC_FILENAME
      KIND_VA_LIST_S = KIND_LISPALLOC_core__VaList_dummy_O, 
      KIND_CONS = KIND_LISPALLOC_core__Cons_O, 
      KIND_CHARACTER = KIND_LISPALLOC_core__Character_dummy_O, 
      KIND_SINGLE_FLOAT = KIND_LISPALLOC_core__SingleFloat_dummy_O, 
      KIND_FIXNUM = KIND_LISPALLOC_core__Fixnum_dummy_O,
      KIND_INSTANCE = KIND_LISPALLOC_core__Instance_O
  } GCKindEnum;
 #undef GC_ENUM
#endif

};







namespace gctools {
  template <class T>
    inline size_t sizeof_with_header();

// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
//
// Define Header and stuff that can exist in the header
//
//

   /*! Stamp - integer value that is written into the header in normal general objects 
               and into the Rack for Instance_O objects.
     See Header_s below for a description of the GC Header tag scheme.
     Stamp needs to fit within a Fixnum.
 */
  typedef uintptr_t Stamp;

/*!

      A header is 8 bytes long and consists of one uintptr_t (8 bytes) value.
      The header ends with a uintptr_t data[0], an array of uintptr_t which intrudes
      into the client data.
      The structure of the header is...
                           kind/stamp      tag
      64 bits total -> |    62 bits     | 2 bits |
      The (header) uintptr_t is a tagged value where the
      two least significant bits are the tag.

      The two least-significant bits of the header uintptr_t value describe the data.
      1r00 == This is an illegal setting for the two lsbs.
      1r01 == This tag indicates that the other bits in the header
      represent a Kind value >> 2 (shifted right 2 bits).
      1r10 == This tag indicates that the header contains a forwarding
      pointer.    The following uintptr_t contains the length of
      the block from the client pointer.
      1r11 == This indicates that the header contains a pad; check the
      bit at 1r0100 to see if the pad is a pad1 (==0) or a pad (==1)

      The KIND is a 62 bit value (0==no kind) that tells
      the MPS GC what the layout of the object is and is used to determine
      IsA relationships between classes.

      The STAMP is a 62 bit value used by generic function dispatch.
      It is the KIND value unless the object is an Instance_O and then it is a value
      assigned by CLOS and is guaranteed to be larger than KIND_max
      Each time a standard-class is redefined a new STAMP is generated and that is written into the Instance_O rack.
    */

 
  class Header_s {
  public:
    static const tagged_kind_t tag_mask   =  BOOST_BINARY(11);
    static const tagged_kind_t invalid_tag=  BOOST_BINARY(00); // indicates not header
    static const tagged_kind_t kind_tag   =  BOOST_BINARY(01); // KIND = tagged_value>>2
    static const tagged_kind_t fwd_tag    =  BOOST_BINARY(10);
    static const tagged_kind_t pad_mask   = BOOST_BINARY(111);
    static const tagged_kind_t pad_test   = BOOST_BINARY(011);
    static const int pad_shift = 3; // 3 bits for pad tag
    static const tagged_kind_t pad_tag    = BOOST_BINARY(011);
    static const tagged_kind_t pad1_tag   = BOOST_BINARY(111);
    static const tagged_kind_t fwd_ptr_mask = ~tag_mask;
    // The kind mask stores 12 bits of info - up to 4096 different KINDs
    //    These are C++ classes managed by the GC.
    static const int kind_shift = 2;
    static const tagged_kind_t kind_mask    = ~0x3; // BOOST_BINARY(11...111111111100);
    static const tagged_kind_t largest_possible_kind = kind_mask>>kind_shift;
  public:
    void validate() const;
    void quick_validate() const;
  public:
    tagged_kind_t header;
#ifdef DEBUG_GUARD
    int tail_start;
    int tail_size;
    tagged_kind_t guard;
#endif
    tagged_kind_t data[0]; // The 0th element intrudes into the client data
  public:
#if !defined(DEBUG_GUARD) 
  Header_s(kind_t k) : header((((kind_t)k) << kind_shift) | kind_tag) {}
#endif
#if defined(DEBUG_GUARD)
    inline void fill_tail() { memset((void*)(((char*)this)+this->tail_start),0xcc,this->tail_size);};
  Header_s(kind_t k,size_t tstart, size_t tsize, size_t total_size) 
    : header((((kind_t)k)<<kind_shift)|kind_tag),
      tail_start(tstart),
        tail_size(tsize),
        guard(0xFEEAFEEBDEADBEEF)
        {
          this->fill_tail();
        };
#endif
      bool invalidP() const { return (this->header & tag_mask) == invalid_tag; };
      bool kindP() const { return (this->header & tag_mask) == kind_tag; };
      bool fwdP() const { return (this->header & tag_mask) == fwd_tag; };
      bool anyPadP() const { return (this->header & pad_test) == pad_tag; };
      bool padP() const { return (this->header & pad_mask) == pad_tag; };
      bool pad1P() const { return (this->header & pad_mask) == pad1_tag; };
  /*! No sanity checking done - this function assumes kindP == true */
      GCKindEnum kind() const { return (GCKindEnum)((this->header&kind_mask) >> kind_shift); };
  /*! setKind wipes out the stamp */
      void setKind(GCKindEnum k) { this->header = (k << kind_shift) | kind_tag; };
  /*! No sanity checking done - this function assumes fwdP == true */
      void *fwdPointer() const { return reinterpret_cast<void *>(this->header & fwd_ptr_mask); };
  /*! Return the size of the fwd block - without the header. This reaches into the client area to get the size */
      void setFwdPointer(void *ptr) { this->header = reinterpret_cast<tagged_kind_t>(ptr) | fwd_tag; };
      tagged_kind_t fwdSize() const { return this->data[0]; };
  /*! This writes into the first tagged_kind_t sized word of the client data. */
      void setFwdSize(size_t sz) { this->data[0] = sz; };
  /*! Define the header as a pad, pass pad_tag or pad1_tag */
      void setPad(tagged_kind_t p) { this->header = p; };
  /*! Return the pad1 size */
      tagged_kind_t pad1Size() const { return alignof(Header_s); };
  /*! Return the size of the pad block - without the header */
      tagged_kind_t padSize() const { return (this->data[0]); };
  /*! This writes into the first tagged_kind_t sized word of the client data. */
      void setPadSize(size_t sz) { this->data[0] = sz; };
  /*! Write the stamp to the stamp bits */
      string description() const {
        if (this->kindP()) {
          std::stringstream ss;
          ss << "Header=" << (void *)(this->header);
          ss << "/";
          ss << obj_name(this->kind());
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
        ss << (void *)(this->header);
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

  /*! global_NextBuiltInStamp starts at KIND_max+1
      See definition in memoryManagement.cc
      This is so that it doesn't use any stamps that were set by the static analyzer. */
  extern std::atomic<Stamp> global_NextStamp;
  /*! Return a new stamp for BuiltIn classes.
      If given != KIND_null then simply return give as the stamp.
      Otherwise return the global_NextBuiltInStamp and advance it
      to the next one */
  void OutOfStamps();
  inline Stamp NextStamp(Stamp given = KIND_null) {
    if ( given != KIND_null ) return given;
    if (global_NextStamp.load() < Header_s::largest_possible_kind) {
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

#ifdef DEBUG_GUARD_EXHAUSTIVE_VALIDATE
#define EXHAUSTIVE_VALIDATE(ptr) (ptr)->quick_validate();
#else
#define EXHAUSTIVE_VALIDATE(ptr)
#endif
  
  inline const void *ClientPtrToBasePtr(const void *mostDerived) {
    const void *ptr = reinterpret_cast<const char *>(mostDerived) - sizeof(Header_s);
    EXHAUSTIVE_VALIDATE(reinterpret_cast<const Header_s*>(ptr));
    return ptr;
  }

  inline void *ClientPtrToBasePtr(void *mostDerived) {
    void *ptr = reinterpret_cast<char *>(mostDerived) - sizeof(Header_s);
    EXHAUSTIVE_VALIDATE(reinterpret_cast<Header_s*>(ptr));
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
      THROW_HARD_ERROR(BF("The client pointer at %p is invalid!\n") % (void *)client);
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

  extern void monitorAllocation(kind_t k, size_t sz);
  extern uint64_t globalBytesAllocated;

#if defined(TRACK_ALLOCATIONS) && defined(DEBUG_SLOW)
  inline void monitor_allocation(kind_t k, size_t sz) {
    globalBytesAllocated += sz;
#ifdef GC_MONITOR_ALLOCATIONS
    if ( global_monitorAllocations.on ) {
      monitorAllocation(k,sz);
    }
#endif
  }
#else
  inline void monitor_allocation(kind_t k, size_t sz) {};
#endif

};

extern "C" {
const char *obj_name(gctools::kind_t kind);
const char *obj_kind_name(core::T_O *ptr);
size_t obj_kind(core::T_O *ptr);
extern void obj_dump_base(void *base);
};

namespace gctools {
/*! Specialize GcKindSelector so that it returns the appropriate GcKindEnum for OT */
  template <class OT>
    struct GCKind {
#ifdef USE_MPS
#ifdef RUNNING_GC_BUILDER
      static GCKindEnum const Kind = KIND_null;
#else
  // We need a default Kind when running the gc-builder.lsp static analyzer
  // but we don't want a default Kind when compiling the mps version of the code
  // to force compiler errors when the Kind for an object hasn't been declared
      static GCKindEnum const Kind = KIND_null; // provide default for weak dependents
#endif // RUNNING_GC_BUILDER
#endif // USE_MPS
#ifdef USE_BOEHM
#ifdef USE_CXX_DYNAMIC_CAST
      static GCKindEnum const Kind = KIND_null; // minimally define KIND_null
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
  class Cons_O;
  class VaList_dummy_O;
  class Instance_O;
}
#if defined(USE_CXX_DYNAMIC_CAST) || defined(RUNNING_GC_BUILDER)
template <> class gctools::GCKind<core::Fixnum_dummy_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Fixnum_dummy_O ;
};
template <> class gctools::GCKind<core::SingleFloat_dummy_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SingleFloat_dummy_O ;
};
template <> class gctools::GCKind<core::Character_dummy_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Character_dummy_O ;
};
template <> class gctools::GCKind<core::Cons_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Cons_O ;
};
template <> class gctools::GCKind<core::VaList_dummy_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__VaList_dummy_O ;
};
template <> class gctools::GCKind<core::Instance_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Instance_O ;
};
#endif

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
    THROW_HARD_ERROR(BF("Bad pointer for SmartPtrToBasePtr"));
    //            ptr = reinterpret_cast<void*>(obj.px_ref());
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


/*! Should be thread_local on linux or __thread on OS X */
#define THREAD_LOCAL thread_local
/*! Declare this in the top namespace */
extern THREAD_LOCAL core::ThreadLocalState *my_thread;
#define RAII_DISABLE_INTERRUPTS() core::RAIIDisableInterrupts disable_interrupts__(my_thread)


namespace gctools {
class GCStack;
GCStack *threadLocalStack();
};

#include <clasp/gctools/gcStack.h>
#include <clasp/gctools/gcalloc.h>

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

#include <clasp/gctools/containers.h>

  

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
    void* _boehm_shadow_memory;
    void* _module_memory;
    size_t _num_entries;

    GCRootsInModule(void* shadow_mem, void* module_mem, size_t num_entries) {
      this->_boehm_shadow_memory = shadow_mem;
      this->_module_memory = module_mem;
      this->_num_entries = num_entries;
    }
    gctools::Tagged set(size_t index, gctools::Tagged val);
    void* address(size_t index) {
      return reinterpret_cast<void*>(&reinterpret_cast<core::T_sp*>(this->_module_memory)[index]);
    }

  };
  
  void initialize_gcroots_in_module(GCRootsInModule* gcroots_in_module, core::T_sp* root_address, size_t num_roots, gctools::Tagged initial_data);
  void shutdown_gcroots_in_module(GCRootsInModule* gcroots_in_module);
};
      

//#endif // _clasp_memoryManagement_H
