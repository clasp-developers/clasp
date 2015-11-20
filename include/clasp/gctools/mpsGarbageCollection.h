/*
    File: mpsGarbageCollection.h
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
#ifndef _clasp_mpsGarbageCollection_H
#define _clasp_mpsGarbageCollection_H

#include <sstream>
#include <type_traits>
#include <boost/config.hpp>
#include <boost/utility/binary.hpp>
#include <clasp/gctools/telemetry.h>

extern "C" {
typedef struct SegStruct *Seg;
typedef mps_arena_t Arena;
typedef mps_addr_t Addr;
extern int SegOfAddr(Seg *segReturn, Arena arena, Addr addr);
//    extern int SegPM(Seg segReturn);
extern void ShieldExpose(Arena arena, Seg seg);
extern void ShieldCover(Arena arena, Seg seg);
};

namespace gctools {

#ifdef DEBUG_MPS_UNDERSCANNING
#define DEBUG_MPS_UNDERSCANNING_TESTS() \
  mps_arena_collect(_global_arena);     \
  mps_arena_release(_global_arena);
#else
#define DEBUG_MPS_UNDERSCANNING_TESTS()
#endif
#ifdef DEBUG_THROW_IF_INVALID_CLIENT_ON
#define DEBUG_THROW_IF_INVALID_CLIENT(c) throwIfInvalidClient(reinterpret_cast<core::T_O *>(c))
#else
#define DEBUG_THROW_IF_INVALID_CLIENT(c)
#endif

struct MpsMetrics {
  size_t finalizationRequests = 0;
  size_t movingAllocations = 0;
  size_t movingZeroRankAllocations = 0;
  size_t nonMovingAllocations = 0;
  size_t unknownAllocations = 0;
  size_t totalMemoryAllocated = 0;
  void movingAllocation(size_t sz) {
    this->totalMemoryAllocated += sz;
    ++this->movingAllocations;
  }
  void movingZeroRankAllocation(size_t sz) {
    this->totalMemoryAllocated += sz;
    ++this->movingZeroRankAllocations;
  }
  void unknownAllocation(size_t sz) {
    this->totalMemoryAllocated += sz;
    ++this->unknownAllocations;
  }
  void nonMovingAllocation(size_t sz) {
    this->totalMemoryAllocated += sz;
    ++this->nonMovingAllocations;
  }
};

extern MpsMetrics globalMpsMetrics;

#define GC_RESULT mps_res_t
#define GC_SCAN_STATE_TYPE mps_ss_t
#define GC_SCAN_STATE ss

class GCObject {
public:
  //	bool isNil() const { return false;};
  //	bool isUnbound() const { return false;};
  //	bool isObject() const { return true;};
  virtual ~GCObject(){};
};

#if !defined(RUNNING_GC_BUILDER)
#define GC_ENUM
typedef
#include STATIC_ANALYZER_PRODUCT //"main/clasp_gc.cc"
    GCKindEnum;
#undef GC_ENUM
#else
typedef enum { KIND_null,
               KIND_max } GCKindEnum;
#endif
};

extern "C" {
char *obj_name(gctools::GCKindEnum kind);
extern void obj_dump_base(void *base);
};

namespace gctools {
template <class T>
GC_RESULT obj_scan_helper(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t &client);
};

extern "C" {

/*! Implemented in gc_interace.cc */
mps_res_t obj_scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit);

/*! Implemented in gc_interace.cc */
mps_addr_t obj_skip(mps_addr_t base);

/*! Implemented in gc_interace.cc */
void obj_finalize(mps_addr_t base);

/*! This must be implemented in the main directory */
extern mps_res_t main_thread_roots_scan(mps_ss_t GC_SCAN_STATE, void *p, size_t s);
};

namespace gctools {

template <class T>
inline size_t sizeof_with_header();

// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
//
// Define Header and stuff that can exist in the header
//
//

/*!

      A Header is 16 bytes long and consists of two uintptr_t (8 bytes) values.
      The first uintptr_t is (header) the second uintptr_t is data[0].
      The (header) uintptr_t is a tagged value where the
      two least significant bits are the tag.

      The two least-significant bits of the header uintptr_t value
      describe the data.
      1r00 == This is an illegal setting for the two lsbs.
              This may be used to indicate that the real header is in the preceeding 16 bytes.
      1r01 == This tag indicates that the other bits in the header
      represent a Kind value >> 2 (shifted right 2 bits).
      1r10 == This tag indicates that the header contains a forwarding
      pointer.    The following uintptr_t contains the length of
      the block from the client pointer.
      1r11 == This indicates that the header contains a pad; check the
      bit at 1r0100 to see if the pad is a pad1 (==0) or a pad (==1)
    */

class Header_s {
public:
  static const uintptr_t tag_mask = BOOST_BINARY(11);
  static const uintptr_t kind_tag = BOOST_BINARY(01); // KIND = tagged_value>>2
  static const uintptr_t fwd_tag = BOOST_BINARY(10);
  static const uintptr_t pad_mask = BOOST_BINARY(111);
  static const uintptr_t pad_test = BOOST_BINARY(011);
  static const uintptr_t pad_tag = BOOST_BINARY(011);
  static const uintptr_t pad1_tag = BOOST_BINARY(111);
  static const uintptr_t fwd_ptr_mask = ~tag_mask;
  //        static const uintptr_t  fwd2_tag        = BOOST_BINARY(001);

private:
  uintptr_t header;
  uintptr_t data[1]; // After this is where the client pointer starts
public:
  Header_s(GCKindEnum k) : header((k << 2) | kind_tag), data{0xDEADBEEF01234567} {};

  bool invalidP() const { return (this->header & tag_mask) == 0; };
  bool kindP() const { return (this->header & tag_mask) == kind_tag; };
  bool fwdP() const { return (this->header & tag_mask) == fwd_tag; };
  bool anyPadP() const { return (this->header & pad_test) == pad_tag; };
  bool padP() const { return (this->header & pad_mask) == pad_tag; };
  bool pad1P() const { return (this->header & pad_mask) == pad1_tag; };

  /*! No sanity checking done - this function assumes kindP == true */
  GCKindEnum kind() const { return (GCKindEnum)(this->header >> 2); };
  void setKind(GCKindEnum k) { this->header = (k << 2) | kind_tag; };
  /*! No sanity checking done - this function assumes fwdP == true */
  void *fwdPointer() const { return reinterpret_cast<void *>(this->header & fwd_ptr_mask); };
  /*! Return the size of the fwd block - without the header. This reaches into the client area to get the size */
  void setFwdPointer(void *ptr) { this->header = reinterpret_cast<uintptr_t>(ptr) | fwd_tag; };
  uintptr_t fwdSize() const { return this->data[0]; };
  /*! This writes into the first uintptr_t sized word of the client data. */
  void setFwdSize(size_t sz) { this->data[0] = sz; };
  /*! Define the header as a pad, pass pad_tag or pad1_tag */
  void setPad(uintptr_t p) { this->header = p; };
  /*! Return the pad1 size */
  uintptr_t pad1Size() const { return sizeof(Header_s); };
  /*! Return the size of the pad block - without the header */
  uintptr_t padSize() const { return data[0]; };
  /*! This writes into the first uintptr_t sized word of the client data. */
  void setPadSize(size_t sz) { this->data[0] = sz; };
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

void headerDescribe(core::T_O *taggedClient);
};

namespace gctools {

constexpr size_t Alignment() {
  return sizeof(Header_s);
};
inline constexpr size_t AlignUp(size_t size) { return (size + Alignment() - 1) & ~(Alignment() - 1); };

template <class T>
inline size_t sizeof_with_header() { return AlignUp(sizeof(T)) + sizeof(Header_s); }
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
namespace gctools {
extern size_t global_sizeof_fwd;
//extern size_t global_alignup_sizeof_header;
inline size_t Align(size_t size) {
  return ((AlignUp(size) >= global_sizeof_fwd) ? AlignUp(size) : global_sizeof_fwd);
};
};

namespace gctools {

#define NON_MOVING_POOL_ALLOCATION_POINT global_non_moving_ap; //_global_mvff_allocation_point

extern mps_arena_t _global_arena;

extern mps_pool_t _global_amc_pool;
//    extern mps_pool_t _global_mvff_pool;
extern mps_pool_t _global_amcz_pool;
extern mps_pool_t global_non_moving_pool;

extern mps_ap_t _global_automatic_mostly_copying_allocation_point;
//    extern mps_ap_t _global_mvff_allocation_point;
extern mps_ap_t _global_automatic_mostly_copying_zero_rank_allocation_point;
extern mps_ap_t global_non_moving_ap;

extern mps_pool_t _global_awl_pool;
extern mps_ap_t _global_weak_link_allocation_point;
extern mps_ap_t _global_strong_link_allocation_point;

template <typename T>
struct GCAllocationPoint {
#ifdef USE_AMC_POOL
  static mps_ap_t get() { return _global_automatic_mostly_copying_allocation_point; };
#define DEFAULT_ALLOCATION_POINT _global_automatic_mostly_copying_allocation_point
#else
  static mps_ap_t get() { return NON_MOVING_POOL_ALLOCATION_POINT; };
#define DEFAULT_ALLOCATION_POINT NON_MOVING_POOL_ALLOCATION_POINT
#endif
};
};

namespace core {
class Cons_O;
};

#ifdef USE_PUT_SELECT_CLASSES_IN_AMC_POOL
//
// Turn this on if you want to allocate just a few classes in an AMC pool
//
namespace gctools {
#define AMC_AP _global_automatic_mostly_copying_allocation_point
template <>
struct allocation_point<core::Cons_O> {
  static mps_ap_t get() { return AMC_AP; };
};
};
#endif

/* ------------------------------------------------------------
   ------------------------------------------------------------

   Macros for fixing pointers managed by GC

   ------------------------------------------------------------
*/

/*! Return the block address of the object pointed to by the smart_ptr */
#define GC_BASE_ADDRESS_FROM_SMART_PTR(_smartptr_) ((_smartptr_).pbase_ref())
#define GC_BASE_ADDRESS_FROM_PTR(_ptr_) (const_cast<void *>(dynamic_cast<const void *>(_ptr_)))

namespace gctools {

inline void *ClientPtrToBasePtr(void *mostDerived) {
  void *ptr = reinterpret_cast<char *>(mostDerived) - sizeof(Header_s);
  return ptr;
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

namespace core {
class T_O;
class WrappedPointer_O;
class Functoid;
class Creator;
class Iterator_O;
};
namespace clbind {
class ConstructorCreator;
};

#ifndef RUNNING_GC_BUILDER
#define DECLARE_FORWARDS
#include STATIC_ANALYZER_PRODUCT
#undef DECLARE_FORWARDS
#endif

namespace gctools {
#if !defined(RUNNING_GC_BUILDER)
#define GC_DYNAMIC_CAST
#include STATIC_ANALYZER_PRODUCT // "main/clasp_gc.cc"
#undef GC_DYNAMIC_CAST
#endif
};

namespace gctools {
template <typename T>
class smart_ptr;
};

inline mps_res_t taggedPtrFix(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, gctools::Tagged *taggedP) {
  if (gctools::tagged_objectp(*taggedP)) {
    gctools::Tagged tagged_obj = *taggedP;
    if (MPS_FIX1(_ss, tagged_obj)) {
      //	    Type* obj(NULL);
      gctools::Tagged obj = gctools::untag_object<gctools::Tagged>(tagged_obj);
      gctools::Tagged tag = gctools::tag<gctools::Tagged>(tagged_obj);
      mps_res_t res = MPS_FIX2(_ss, reinterpret_cast<mps_addr_t *>(&obj));
      if (res != MPS_RES_OK)
        return res;
      obj = obj | tag;
#ifdef DEBUG_TELEMETRY
      // Telemetry only on pointer fixes that change
      if (tagged_obj != obj) {
        GC_TELEMETRY3(telemetry::label_smart_ptr_fix,
                      (uintptr_t)taggedP,
                      (uintptr_t)tagged_obj,
                      (uintptr_t)obj);
      }
#endif
      *taggedP = obj;
    }
  };
  return MPS_RES_OK;
};

#define SMART_PTR_FIX(_smartptr_) taggedPtrFix(_ss, _mps_zs, _mps_w, _mps_ufs, _mps_wt, reinterpret_cast<gctools::Tagged *>(&((_smartptr_).rawRef_())))

inline mps_res_t ptrFix(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, gctools::Tagged *taggedP) {
  if (gctools::tagged_objectp(*taggedP)) {
    gctools::Tagged tagged_obj = *taggedP;
    if (MPS_FIX1(_ss, tagged_obj)) {
      gctools::Tagged obj = gctools::untag_object<gctools::Tagged>(tagged_obj);
      gctools::Tagged tag = gctools::tag<gctools::Tagged>(tagged_obj);
      mps_res_t res = MPS_FIX2(_ss, reinterpret_cast<mps_addr_t *>(&obj));
      if (res != MPS_RES_OK)
        return res;
      obj = obj | tag;
#ifdef DEBUG_TELEMETRY
      // Telemetry only on pointer fixes that change
      if (tagged_obj != obj) {
        GC_TELEMETRY3(telemetry::label_tagged_pointer_fix,
                      (uintptr_t)taggedP,
                      (uintptr_t)tagged_obj,
                      (uintptr_t)obj);
      }
#endif
      *taggedP = obj;
    };
  } else if (*taggedP) {
    printf("%s:%d POINTER_FIX called on untagged pointer\n", __FILE__, __LINE__);
    gctools::Tagged obj = *taggedP;
    if (MPS_FIX1(_ss, obj)) {
      mps_res_t res = MPS_FIX2(_ss, reinterpret_cast<mps_addr_t *>(&obj));
      if (res != MPS_RES_OK)
        return res;
      *taggedP = obj;
    };
  };
  return MPS_RES_OK;
};
#define TAGGED_POINTER_FIX(_ptr_) ptrFix(_ss, _mps_zs, _mps_w, _mps_ufs, _mps_wt, reinterpret_cast<gctools::Tagged *>(&(_ptr_).rawRef_()))
#define SIMPLE_POINTER_FIX(_ptr_) ptrFix(_ss, _mps_zs, _mps_w, _mps_ufs, _mps_wt, reinterpret_cast<gctools::Tagged *>(&_ptr_))

namespace gctools {

/*! Initialize the memory pool system and call the startup function which
      has the type: int startup(int argc, char* argv[]) just like main.
      Also pass an optional object-format for MPS
    */
int initializeMemoryPoolSystem(MainFunctionType startup, int argc, char *argv[], mps_fmt_auto_header_s *mps_fmt, bool mpiEnabled, int mpiRank, int mpiSize);

/*! Search the heap and the stack for an address and print hits
      This can't currently be called from within obj_skip - so it's not
      useful.    Come up with another way to determine ownership of pointers */
void searchHeapAndStackForAddress(mps_addr_t addr);
};

namespace gctools {
class GCStack;
void mpsAllocateStack(GCStack *stack);
void mpsDeallocateStack(GCStack *stack);
};

extern "C" {

/*! Return the number of messages processed */
extern int processMpsMessages(void);
};

#endif // _clasp_memoryPoolSystem_H
