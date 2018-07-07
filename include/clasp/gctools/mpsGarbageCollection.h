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
  void mps_regsiter_root_address(gctools::Tagged* ptr);
}

namespace gctools {

  extern bool global_underscanning;
#ifdef DEBUG_MPS_UNDERSCANNING
#define DEBUG_MPS_UNDERSCANNING_TESTS() \
  if ( global_underscanning ) { \
    mps_arena_collect(global_arena);     \
    mps_arena_release(global_arena); \
  }
#else
#define DEBUG_MPS_UNDERSCANNING_TESTS()
#endif
#ifdef DEBUG_THROW_IF_INVALID_CLIENT_ON
#define DEBUG_THROW_IF_INVALID_CLIENT(c) throwIfInvalidClient(reinterpret_cast<core::T_O *>(c))
#else
#define DEBUG_THROW_IF_INVALID_CLIENT(c)
#endif

struct MpsMetrics {
  std::atomic<size_t> finalizationRequests;
  std::atomic<size_t> nonMovingAllocations;
  std::atomic<size_t> movingAllocations;
  std::atomic<size_t> movingZeroRankAllocations;
  std::atomic<size_t> consAllocations;
  std::atomic<size_t> unknownAllocations;
  std::atomic<size_t> totalMemoryAllocated;
  
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
};

namespace gctools {
template <class T>
GC_RESULT obj_scan_helper(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t &client);
};

extern "C" {

void my_mps_finalize(void* client);

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
  void mps_register_roots(void* root_address, size_t num_roots);
};


extern "C" {
extern mps_arena_t global_arena;
}

namespace gctools {

//#define NON_MOVING_POOL_ALLOCATION_POINT global_non_moving_allocation_point; //_global_mvff_allocation_point

extern mps_pool_t global_amc_pool;
extern mps_pool_t global_amc_cons_pool;
//    extern mps_pool_t _global_mvff_pool;
extern mps_pool_t global_amcz_pool;
extern mps_pool_t global_non_moving_pool;
//extern mps_pool_t global_unmanaged_pool;
extern mps_pool_t global_awl_pool;


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
template <typename T>
class smart_ptr;
};

inline mps_res_t taggedPtrFix(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, gctools::Tagged *taggedP) {
  if (gctools::tagged_objectp(*taggedP)) {
    gctools::Tagged tagged_obj = *taggedP;
    if (MPS_FIX1(_ss, tagged_obj)) {
      gctools::Tagged obj = gctools::untag_object<gctools::Tagged>(tagged_obj);
      gctools::Tagged tag = gctools::tag<gctools::Tagged>(tagged_obj);
      mps_res_t res = MPS_FIX2(_ss, reinterpret_cast<mps_addr_t *>(&obj));
      if (res != MPS_RES_OK) return res;
      obj = obj | tag;
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
      if (res != MPS_RES_OK) return res;
      obj = obj | tag;
      *taggedP = obj;
    };
  };
  return MPS_RES_OK;
};
#define TAGGED_POINTER_FIX(_ptr_) ptrFix(_ss, _mps_zs, _mps_w, _mps_ufs, _mps_wt, reinterpret_cast<gctools::Tagged *>(&(_ptr_).rawRef_()))
// Get rid of SIMPLE_POINTER_FIX - its a terrible name
#define SIMPLE_POINTER_FIX(_ptr_) ptrFix(_ss, _mps_zs, _mps_w, _mps_ufs, _mps_wt, reinterpret_cast<gctools::Tagged *>(&(_ptr_)))
#define POINTER_REF_FIX(_ptr_) ptrFix(_ss, _mps_zs, _mps_w, _mps_ufs, _mps_wt, reinterpret_cast<gctools::Tagged *>(&(_ptr_)))
#define POINTER_FIX(_ptr_) ptrFix(_ss, _mps_zs, _mps_w, _mps_ufs, _mps_wt, reinterpret_cast<gctools::Tagged *>(_ptr_))

namespace gctools {

/*! Initialize the memory pool system and call the startup function which
      has the type: int startup(int argc, char* argv[]) just like main.
      Also pass an optional object-format for MPS
    */
  int initializeMemoryPoolSystem(MainFunctionType startup, int argc, char *argv[], bool mpiEnabled, int mpiRank, int mpiSize);

/*! Search the heap and the stack for an address and print hits
      This can't currently be called from within obj_skip - so it's not
      useful.    Come up with another way to determine ownership of pointers */
void searchHeapAndStackForAddress(mps_addr_t addr);
};

namespace gctools {
class GCStack;
void mpsAllocateStack(GCStack *stack);
void mpsDeallocateStack(GCStack *stack);

 void my_mps_thread_reg(mps_thr_t* threadP);
 void my_mps_thread_deref(mps_thr_t thread);

};

extern "C" {

/*! Return the number of messages processed and the number of finalization messages */
extern size_t processMpsMessages(size_t& finalizations);
};

namespace core {
  class ThreadLocalState;
};


namespace gctools {

struct ThreadLocalAllocationPoints {
  void initializeAllocationPoints();
  void destroyAllocationPoints();
    mps_ap_t _automatic_mostly_copying_allocation_point;
    mps_ap_t _amc_cons_allocation_point;
    mps_ap_t _automatic_mostly_copying_zero_rank_allocation_point;
    mps_ap_t _non_moving_allocation_point;
    mps_ap_t _weak_link_allocation_point;
    mps_ap_t _strong_link_allocation_point;
  // Custom allocators
  mps_ap_t   _custom_allocation_points[MAX_CUSTOM_ALLOCATION_POINTS];
};

 extern THREAD_LOCAL ThreadLocalAllocationPoints my_thread_allocation_points;

};
#endif // _clasp_memoryPoolSystem_H
